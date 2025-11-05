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
    let bident = syn::Ident::new(&bname, ast.ident.span());
    // tokens
    // eprintln!("{:#?}", ast);
    let output = //  proc_macro2::TokenStream
        quote! {
            struct #bident {
            }

            impl #name {
               fn builder() -> #bident {
                    return #bident {}
                }
            }
        };

    output.into()
}
