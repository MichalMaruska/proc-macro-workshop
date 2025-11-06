#![feature(assert_matches)]
use proc_macro::TokenStream;
use std::assert_matches::assert_matches;


// Better?
// mmc: I don't understand ... https://docs.rs/proc-macro2/1.0.103/proc_macro2/
// > If parsing with Syn, you’ll use parse_macro_input! instead to propagate
// > parse errors correctly back to the compiler when parsing fails.
// so it's even better?
use syn::{parse_macro_input, DeriveInput};
use syn::{Meta,Expr,Lit,ExprLit, ExprPath ,Attribute};
use quote::quote;

// at 1:26 he simplifies a lot. to meet another requirement
//  1:30   returns Option to avoid 2 methods.
//  1:35  here

fn ty_inner_type<'t> (outer: &'_ str, ty: &'t syn::Type) -> Option<&'t syn::Type> {

    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != outer {
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
fn builder_attribute(f: &syn::Field) -> Option<&Attribute> {

    for attr in &f.attrs {
        // we want it Outer -- in front of the field name
        if attr.style == syn::AttrStyle::Outer {};
        // it's not a path, but MetaList
        if attr.path().segments.len() == 1 &&
            attr.path().is_ident("builder") {
                return Some(attr)
            }
    }
    None
}

fn extract_builder(field_name: &syn::Ident, ty: &syn::Type, attr: &Attribute) -> (bool, proc_macro2::TokenStream) {

    if let Meta::List(_) = attr.meta {
    } else {
        panic!("not a list?")
    }

    let assignment: Expr = attr.parse_args().unwrap(); // fixme!

    if let Expr::Assign(assign) = assignment {

        if let Expr::Path( ExprPath{ref path, ..} ) = *assign.left {
            if !path.is_ident("each") {
                //assert_eq!(path.get_ident(), "each");
            }
        }


        if let Expr::Lit( ExprLit{lit: Lit::Str(ref strlit), ..} ) = *assign.right {
            // dbg!(&strlit);
            if let Some(inner_ty) = ty_inner_type("Vec", ty) {

                let ident = proc_macro2::Ident::new(
                    &strlit.value(),
                    attr.path().segments.first().unwrap().ident.span(),
                );

                return (
                    &field_name.to_string() == &strlit.value(),

                    quote! {
                        pub fn #ident (&mut self, value: #inner_ty) -> &mut Self {
                            self.#field_name.push(value);
                            self
                        }
                    }
                )
            } else {
                panic!("not a Vector");
            }}}
    panic!("not an assignment")
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

    // Option -> avoid adding Option
    // Vector -> as well?
    let optionized = fields.iter().map(|f| { // over Field
        let name = &f.ident;
        let ty = &f.ty;

        if ty_inner_type("Option", ty).is_some() || builder_attribute(f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    // the builder:
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        // mapping: Struct -> StructBuilder
        if ty_inner_type("Option", ty).is_some() || builder_attribute(f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    // iterator
    // recognize when attribute says to create a function, and maybe suppress
    // the usual one:

    // now stamp out
    // 1 when attribute -> drop Vector<>
    // same name? -- skip

    let methods = fields.iter().map(|f| {

        // let name = &f.ident.unwrap();
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let mut same_name: bool = false;

        let mut single_adder  = None;

        if let Some(ref attr) = builder_attribute(f) {
            let (same, custom_setter) = extract_builder(&name, ty, attr);
            same_name = same;
            // in this case I can assume it's Vec
            single_adder = Some(custom_setter);
        }

        if same_name {
            return single_adder.unwrap();
        } else {
            // maybe both
            let inner_ty = ty_inner_type("Option", ty);
            let setter =
                if inner_ty.is_some() {

                    let raw_type = inner_ty.unwrap();
                    quote! {
                        pub fn #name ( & mut self, #name: #raw_type ) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }
                } else if builder_attribute(f).is_some() { // not same_name
                    quote! {
                        pub fn #name ( & mut self, #name: #ty) -> &mut Self {
                            self.#name = #name;
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
                };
            if single_adder.is_some() {
                let fun = single_adder.unwrap();
                quote!(
                    #fun
                    #setter
                )
            } else {
                setter
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_attribute(f).is_some() {
            quote! { #name: Vec::new()}
        } else {
            quote! { #name: None}
        }
    });

    // tokens
    // eprintln!("{:#?}", ast);
    let output = //  proc_macro2::TokenStream
        quote! {
            pub struct #builder_ident {
                #(#optionized,)*
            }

            // #(#extend_methods,)*;
            impl #builder_ident {
                #(#methods)*

                pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                    Ok(#name {
                        #(#build_fields,)*
                    })
                }
            }

            impl #name {
                fn builder() -> #builder_ident {
                    return #builder_ident {
                        #(#build_empty,)*
                    }
                }
            }
        };

    output.into()
}
