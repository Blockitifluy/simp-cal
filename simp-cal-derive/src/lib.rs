use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Data, DeriveInput, Ident, LitChar, LitInt, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

fn get_variant_refs(input: &DeriveInput) -> Option<Vec<&syn::Variant>> {
    match &input.data {
        Data::Enum(data_enum) => Some(data_enum.variants.iter().collect()),
        _ => None,
    }
}

struct ParseOperator {
    pub sym: char,
    pub binding_power: u8,
}

impl Parse for ParseOperator {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let sym_ident: Ident = input.parse()?;
        if sym_ident != "sym" {
            return Err(input.error("expected `sym`"));
        }
        input.parse::<Token![=]>()?;
        let sym_lit: LitChar = input.parse()?;
        let sym = sym_lit.value();

        input.parse::<Token![,]>()?;

        let bp_ident: Ident = input.parse()?;
        if bp_ident != "bind" {
            return Err(input.error("expected `bind`"));
        }
        input.parse::<Token![=]>()?;
        let bp_lit: LitInt = input.parse()?;
        let binding_power = bp_lit.base10_parse::<u8>()?;

        if !input.is_empty() {
            return Err(input.error("unexpected extra tokens"));
        }

        Ok(Self { sym, binding_power })
    }
}

#[derive(Debug)]
struct OperatorVar {
    pub sym: char,
    pub binding_power: u8,
    pub ident: Ident,
}

#[proc_macro_derive(OperatorTrait, attributes(operator))]
pub fn derive_operator_impl(input: TokenStream) -> TokenStream {
    let item_enum = parse_macro_input!(input as DeriveInput);

    let DeriveInput { ident, .. } = item_enum.clone();

    let Some(variants) = get_variant_refs(&item_enum) else {
        panic!(""); // TODO:
    };

    let mut op_var = Vec::with_capacity(2);

    for var in variants {
        let mut has_attr = false;

        for attr in &var.attrs {
            if !attr.path().is_ident("operator") {
                continue;
            }

            let meta_list = attr
                .meta
                .require_list()
                .expect("operator attribute not meta list");
            let meta_args = meta_list
                .parse_args::<ParseOperator>()
                .expect("expected two literals");

            op_var.push(OperatorVar {
                sym: meta_args.sym,
                binding_power: meta_args.binding_power,
                ident: var.ident.clone(),
            });
            has_attr = true;
            break;
        }

        if !has_attr {
            panic!("needs operator attr");
        }
    }

    let op_sign = op_var.iter().map(|op| {
        let sym = op.sym;
        let ident = &op.ident;

        quote! {
            #sym => Some(Self::#ident),
        }
    });

    let as_sign = op_var.iter().map(|op| {
        let sym = op.sym;
        let ident = &op.ident;

        quote! {
            Self::#ident => #sym,
        }
    });

    let binding = op_var.iter().map(|op| {
        let bind = op.binding_power;
        let ident = &op.ident;

        quote! {
            Self::#ident => #bind,
        }
    });

    quote!(
        impl OperatorTrait for #ident {
            fn get_operator_from_sign(sign: char) -> Option<Self> {
                match sign {
                   #(#op_sign)*
                    _ => None
                }
            }

            fn as_sign(&self) -> char {
                match self {
                    #(#as_sign)*
                }
            }

            fn get_binding_power(&self) -> BindPower {
                match self {
                    #(#binding)*
            }
            }
        }
    )
    .into()
}
