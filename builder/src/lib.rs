use proc_macro::TokenStream;

// Better?
// mmc: I don't understand ... https://docs.rs/proc-macro2/1.0.103/proc_macro2/
// > If parsing with Syn, you’ll use parse_macro_input! instead to propagate
// > parse errors correctly back to the compiler when parsing fails.
// so it's even better?
use syn::{parse_macro_input, DeriveInput};
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


#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {

    let ast = parse_macro_input!(input as DeriveInput); // tokens ... > tree.

    // we expect this:
    if let syn::Data::Struct(ref dataStruct) = ast.data {
    } else {
        panic!("not a struct");
    }

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
