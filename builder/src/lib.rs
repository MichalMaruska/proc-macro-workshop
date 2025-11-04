#![feature(assert_matches)]
use proc_macro::TokenStream;
use std::assert_matches::assert_matches;


// Better?
// mmc: I don't understand ... https://docs.rs/proc-macro2/1.0.103/proc_macro2/
// > If parsing with Syn, you’ll use parse_macro_input! instead to propagate
// > parse errors correctly back to the compiler when parsing fails.
// so it's even better?
use syn::{parse_macro_input, DeriveInput};
use syn::{Meta,Expr,Lit,ExprLit, Path};
use quote::quote;

// at 1:26 he simplifies a lot. to meet another requirement
//  1:30   returns Option to avoid 2 methods.
//  1:35  here

fn ty_inner_type(ty: &syn::Type) -> Option<&syn::Type> {

    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != "Option" {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();

            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

// Based on what I see in the dump
// returns each = "ident"
fn extract_attribute(f: &syn::Field) -> Option<proc_macro2::Ident> {
    let name = "each";

    for attr in &f.attrs {
        // we want it Outer -- in front of the field name
        if attr.style == syn::AttrStyle::Outer {};
        // it's not a path, but MetaList
        if attr.path().segments.len() == 1 &&
            attr.path().is_ident("builder") {
                // tokens[Ident = Literal]
                // ident = "each"
                // literal ... name of the function.
                eprintln!("Found: {:#?}", attr); // f.attrs

                if let Meta::List(ref list) = attr.meta {
                    eprintln!("extract: {:#?}", list);
                    // let atrr.parse_args()
                    // assert_eq!(list.tokens.Ident, "each");
                } else {
                    panic!("not a list?")
                }
                // if let list attr.path()
                // parse_args_with

                let assignment: Expr = attr.parse_args().unwrap(); // fixme!

                if let Expr::Assign(assign) = assignment {
                    // ExprAssign
                    // fixme: eprintln!("it's an assignment: {:#?}", assign);
                    dbg!(&assign.left);
                    dbg!(&assign.right);

                    dbg!(& attr.path().segments.first().unwrap().ident);

                    // let Expr::Lit{ syn::ExprLit(lit: Lit::Str(ref strlit))} = *assign.right
                    // Expr
                    // syn::ExprLit
                    // ExprLit(ref lit )


                    if let Expr::Lit( ExprLit{lit: Lit::Str(ref strlit), ..} ) = *assign.right {
                        dbg!(&strlit);

                        if let Expr::Path(ref i) = *assign.left {
                            return Some(
                                proc_macro2::Ident::new(
                                    &strlit.value(),
                                    attr.path().segments.first().unwrap().ident.span(),
                                )
                            )
                        }}
                } else {
                    panic!("not an assignment")
                }
            }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {

    let ast = parse_macro_input!(input as DeriveInput); // tokens ... > tree.

    // we expect this:
    assert_matches!(ast.data, syn::Data::Struct(_));

    let name = &ast.ident;
    let bname = format!("{name}Builder");
    let builder_ident = syn::Ident::new(&bname, ast.ident.span());

    let fields = if
        let syn::Data::Struct(syn::DataStruct
                              { fields:  syn::Fields::Named (
                                  syn::FieldsNamed {
                                      // this is ref to a vector.
                                      ref named, ..}), .. }) = ast.data {
            // so this is a &vector<Field,Token>
            named
        } else {
            unimplemented!()
        };

    let optionized = fields.iter().map(|f| { // over Field
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type(ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if ty_inner_type(ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = ty_inner_type(ty) {
            quote! {
                pub fn #name ( & mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name ( & mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }});

    // iterator
    // recognize when attribute says to create a function, and maybe suppress
    // the usual one:
    let extend_methods = fields.iter().filter_map(|f| {
        extract_attribute(f)
    });

    let a: Vec<_> = extend_methods.collect();
    // tokens
    // eprintln!("{:#?}", ast);
    let output = //  proc_macro2::TokenStream
        quote! {
            pub struct #builder_ident {
                #(#optionized,)*
            }

            impl #builder_ident {
                #(#methods)*

                pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                    Ok(#name {
                        #(#build_fields,)*
                    })
                }
            }

            impl #name {
                fn builder() -> #builder_ident {
                    return #builder_ident {
                        executable: None,
                        current_dir: None,
                        env: None,
                        args: None,
                    }
                }
            }
        };

    output.into()
}
