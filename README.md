TreeScan for conditonal baseline risk
================

# TreeScan Statistics with Baseline Risk Consideration

## Overview
This repository implements an enhanced version of TreeScan statistics that incorporates baseline risk considerations for comparing adverse event rates between different medications. The traditional TreeScan method, based on the Bernoulli model, allows for comparing either pre-post rates for a single medication or adverse event rates between two medications. However, this implementation addresses a critical gap in the current methodology by accounting for baseline risk when comparing pre-post changes between different medications.

## Background

### Current Limitations
The existing TreeScan statistical methods can:
- Compare pre- and post-medication adverse event rates for a single medication
- Compare adverse event rates between two different medications
- Utilize Bernoulli model-based statistical analysis

However, they cannot effectively compare differences in pre-post change rates between two medications while accounting for baseline risk.

### The Problem of Baseline Risk
Individual baseline risk is a crucial factor that current methods don't adequately address. This can lead to biased results when:
- Individuals have pre-existing conditions that could cause adverse events regardless of medication
- The study population has an uneven distribution of baseline risk factors between medication groups

## Example: COVID-19 Vaccine Analysis
Consider comparing myocarditis/pericarditis rates between two COVID-19 vaccines:

### Current Method Limitations
- Only compares post-vaccination occurrence rates
- Cannot distinguish between:
  - Pre-existing condition-related occurrences
  - Vaccine-induced occurrences
- May produce biased results if one vaccine group has more participants with relevant medical history

## Proposed Solution
This implementation introduces a new statistical approach that:
1. Incorporates individual baseline risk factors
2. Enables accurate comparison of pre-post changes between different medications
3. Reduces bias in comparative analyses
4. Provides more reliable risk assessment for different treatment options

## Future Development
We welcome contributions to enhance this methodology, particularly in:
- Improving baseline risk assessment methods
- Developing more sophisticated statistical models
- Creating visualization tools for risk comparison
- Implementing additional validation methods

## Installation and Usage
[To be added]

## Contributing
[To be added]

## License
[To be added]
