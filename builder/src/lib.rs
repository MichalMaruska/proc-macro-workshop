use proc_macro::TokenStream;

// Better?
// mmc: I don't understand ... https://docs.rs/proc-macro2/1.0.103/proc_macro2/
// > If parsing with Syn, you’ll use parse_macro_input! instead to propagate
// > parse errors correctly back to the compiler when parsing fails.
// so it's even better?
use syn::{parse_macro_input, DeriveInput};
use syn::parse::{Parse};  // will use this trait!

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {

    // even better?
    let _input = parse_macro_input!(input as DeriveInput);
    // tokens

    /* transform input */
    let output: proc_macro2::TokenStream = proc_macro2::TokenStream::new();

    proc_macro::TokenStream::from(output)
}
