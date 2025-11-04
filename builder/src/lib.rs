use proc_macro::TokenStream;

// Better?
// mmc: I don't understand ... https://docs.rs/proc-macro2/1.0.103/proc_macro2/
// > If parsing with Syn, you’ll use parse_macro_input! instead to propagate
// > parse errors correctly back to the compiler when parsing fails.
// so it's even better?
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {

    // even better?
    let ast = parse_macro_input!(input as DeriveInput); // tokens ... > tree.
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
        quote! { #name: std::option::Option<#ty> }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            pub fn #name ( & mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
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
