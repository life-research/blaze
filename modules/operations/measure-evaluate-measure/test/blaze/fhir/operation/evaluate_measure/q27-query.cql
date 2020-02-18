library Retrieve
using FHIR version '4.0.0'
include FHIRHelpers version '4.0.0'

valueset "C25": 'https://fhir.bbmri.de/ValueSet/icd-10-gm-c25'

define InInitialPopulation:
  exists([Condition: "C25"])
