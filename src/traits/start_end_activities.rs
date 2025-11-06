use ebi_arithmetic::Fraction;
use intmap::IntMap;

use crate::Activity;

pub trait StartEndActivities {
    fn start_activites(&self) -> IntMap<Activity, Fraction>;
    
    fn end_activites(&self) -> IntMap<Activity, Fraction>;
}