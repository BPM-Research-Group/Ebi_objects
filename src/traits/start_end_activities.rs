use crate::Activity;
use ebi_bpmn::ebi_arithmetic::Fraction;
use intmap::IntMap;

pub trait StartEndActivities {
    fn start_activities(&self) -> IntMap<Activity, Fraction>;

    fn end_activities(&self) -> IntMap<Activity, Fraction>;
}
