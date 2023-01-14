use std::collections::HashSet;
use proc_macro2::{
    Span,
    TokenStream,
};
use quote::{
    format_ident,
    quote,
    ToTokens,
};
use syn::{
    self,
    Ident,
    Lifetime,
    DeriveInput,
};

fn samevariant_(args: TokenStream, body: TokenStream) -> TokenStream {
    let ident =
        syn::parse2::<Ident>(args).expect("Needs a single argument, the name of the new enum to geenerate");
    let nonmatching = format_ident!("Nonmatching");
    let ast = syn::parse2::<DeriveInput>(body).unwrap();

    // Copy base enum visibility
    let vis = ast.vis;

    // Append new lifetime onto existing lifetimes
    let (life, generics_a, generics_b, generic_forward, base_generic_forward) = if ast.generics.params.is_empty() {
        (Lifetime::new("'l", Span::call_site()), quote!(< 'l >), quote!(), quote!(< 'l >), quote!())
    } else {
        let mut seen_lifetimes = HashSet::new();
        let mut generics_a = Vec::new();
        let mut base_generic_forward = Vec::new();
        for p in ast.generics.params {
            match &p {
                syn::GenericParam::Type(t) => {
                    let ident = &t.ident;
                    base_generic_forward.push(quote!(#ident));
                },
                syn::GenericParam::Lifetime(l) => {
                    let l1 = &l.lifetime;
                    base_generic_forward.push(quote!(#l1));
                    seen_lifetimes.insert(l1.ident.to_string());
                },
                syn::GenericParam::Const(c) => {
                    let ident = &c.ident;
                    base_generic_forward.push(quote!(#ident));
                },
            };
            generics_a.push(p.to_token_stream());
        }
        let mut generic_forward = Vec::new();
        generic_forward.extend(base_generic_forward.iter().map(|x| x.clone()));
        let mut life = None;
        for i in 0usize.. {
            let name = format!("l{}", i);
            if seen_lifetimes.contains(&name) {
                continue;
            }
            life = Some(Lifetime::new(&format!("'{}", name), Span::call_site()));
            break;
        }
        let life = life.unwrap();
        generic_forward.push(life.to_token_stream());
        generics_a.push(life.to_token_stream());
        let generics_b = match &ast.generics.where_clause {
            Some(w) => w.to_token_stream(),
            None => quote!(),
        };
        (
            life,
            quote!(< #(#generics_a,) *>),
            generics_b,
            quote!(< #(#generic_forward,) *>),
            quote!(< #(#base_generic_forward,) *>),
        )
    };

    // Build new stuff
    let base_ident = ast.ident;
    let mut fields = vec![];
    let mut matches = vec![];
    match &ast.data {
        syn::Data::Struct(_) => panic!("Must be used on enum, not struct"),
        syn::Data::Union(_) => panic!("Must be used on enum, not union"),
        syn::Data::Enum(e) => {
            for ele in &e.variants {
                let base_var_ident = &ele.ident;
                let var_ident = if ele.ident == nonmatching {
                    format_ident!("{}{}", base_ident, ele.ident)
                } else {
                    ele.ident.clone()
                };
                match &ele.fields {
                    syn::Fields::Named(_) => panic!(
                        "Only tuple/unit enum variants are supported, but {} is a struct-like variant",
                        ele.ident
                    ),
                    syn::Fields::Unnamed(t) => {
                        let t = if t.unnamed.len() == 1 {
                            t.unnamed.to_token_stream()
                        } else {
                            t.to_token_stream()
                        };
                        fields.push(quote!(#var_ident(& #life #t, & #life #t)));
                        matches.push(
                            quote!(
                                (
                                    #base_ident:: #base_var_ident(a),
                                    #base_ident:: #base_var_ident(b)
                                ) => #ident:: #var_ident(a, b)
                            ),
                        );
                    },
                    syn::Fields::Unit => {
                        fields.push(quote!(#var_ident));
                        matches.push(
                            quote!(
                                (#base_ident:: #base_var_ident, #base_ident:: #base_var_ident) => #ident:: #var_ident
                            ),
                        );
                    },
                }
            }
        },
    }

    // Assemble
    quote!{
        #vis enum #ident #generics_a #generics_b {
            #(
                #fields,
            ) * #nonmatching(& #life #base_ident #base_generic_forward, & #life #base_ident #base_generic_forward),
        }
        impl #generics_a #ident #generic_forward #generics_b {
            #vis fn pairs(
                a:& #life #base_ident #base_generic_forward,
                b:& #life #base_ident #base_generic_forward,
            ) -> Self {
                match(a, b) {
                    #(#matches,) *(a, b) => #ident:: Nonmatching(a, b),
                }
            }
        }
    }
}

#[proc_macro_attribute]
pub fn samevariant(args: proc_macro::TokenStream, body: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let body: TokenStream = body.into();
    let out = samevariant_(args.into(), body.clone());
    quote!(#body #out).into()
}

#[cfg(test)]
mod tests {
    use crate::samevariant_;
    use proc_macro2::TokenStream;
    use quote::quote;

    fn fmt(a: TokenStream) -> String {
        genemichaels::format_str(&a.to_string(), &genemichaels::FormatConfig { ..Default::default() })
            .unwrap()
            .rendered
    }

    #[test]
    fn basic() {
        let got = fmt(samevariant_(quote!(ABCSV), quote!{
            pub enum ABC {
                A,
                B,
                C(i32),
            }
        }));
        let want = fmt(quote!{
            pub enum ABCSV<'l> {
                A,
                B,
                C(&'l i32, &'l i32),
                Nonmatching(&'l ABC, &'l ABC),
            }

            impl<'l> ABCSV<'l> {
                pub fn pairs(a: &'l ABC, b: &'l ABC) -> Self {
                    match (a, b) {
                        (ABC::A, ABC::A) => ABCSV::A,
                        (ABC::B, ABC::B) => ABCSV::B,
                        (ABC::C(a), ABC::C(b)) => ABCSV::C(a, b),
                        (a, b) => ABCSV::Nonmatching(a, b),
                    }
                }
            }
        });
        assert!(got == want, "Mismatch:\nWant:\n{}\n\nGot:\n{}\n", want, got);
    }

    #[test]
    fn inherit_lifetimes() {
        let got = fmt(samevariant_(quote!(ABCSV), quote!{
            pub enum ABC<'a> {
                A(PhantomData<'a>),
            }
        }));
        let want = fmt(quote!{
            pub enum ABCSV<'a, 'l0> {
                A(&'l0 PhantomData<'a>, &'l0 PhantomData<'a>),
                Nonmatching(&'l0 ABC<'a>, &'l0 ABC<'a>),
            }

            impl<'a, 'l0> ABCSV<'a, 'l0> {
                pub fn pairs(a: &'l0 ABC<'a>, b: &'l0 ABC<'a>) -> Self {
                    match (a, b) {
                        (ABC::A(a), ABC::A(b)) => ABCSV::A(a, b),
                        (a, b) => ABCSV::Nonmatching(a, b),
                    }
                }
            }
        });
        assert!(got == want, "Mismatch:\nWant:\n{}\n\nGot:\n{}\n", want, got);
    }
}
