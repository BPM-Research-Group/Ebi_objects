use ebi_arithmetic::Fraction;
use intmap::IntMap;

use crate::Activity;

pub trait StartEndActivities {
    fn start_activities(&self) -> IntMap<Activity, Fraction>;
    
    fn end_activities(&self) -> IntMap<Activity, Fraction>;
}