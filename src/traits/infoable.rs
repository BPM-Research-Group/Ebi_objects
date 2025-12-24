use anyhow::Result;
use ebi_arithmetic::{Fraction, MaybeExact};
use malachite::{base::num::logic::traits::SignificantBits, rational::Rational};

pub trait Infoable {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()>;
}

impl Infoable for String {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        Ok(writeln!(f, "Length\t{}", self.len())?)
    }
}

impl Infoable for Rational {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        Ok(write!(f, "{} bits", self.significant_bits(),)?)
    }
}

impl Infoable for Fraction {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match (self.approx_ref(), self.exact_ref()) {
            (Ok(_), Ok(fr)) => fr.info(f),
            (Ok(fr), Err(_)) => Ok(writeln!(f, "Approximate value\t{}", fr)?),
            (Err(_), Ok(fr)) => {
                fr.info(f)?;
                Ok(writeln!(f, "")?)
            }
            (Err(_), Err(_)) => Ok(writeln!(
                f,
                "Fraction is a result of combining exact and approximate arithmethic and therefore has no value."
            )?),
        }
    }
}