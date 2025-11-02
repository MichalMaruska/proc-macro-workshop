use proc_macro::TokenStream;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let _input = proc_macro2::TokenStream::from(input);

    /* transform input */
    let output: proc_macro2::TokenStream = proc_macro2::TokenStream::new();

    proc_macro::TokenStream::from(output)
}
