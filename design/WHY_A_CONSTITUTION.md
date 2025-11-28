# Why CausalStress Has a “Constitution”
*A practical note — not a grand statement*

CausalStress includes a document we call the **Constitution**.  
The name may sound big, but the intention is modest:

> **We needed a clear, stable set of rules so the science doesn’t drift.**

Simulation studies in causal inference often suffer from subtle inconsistencies:

- truth is defined differently in every paper  
- DGPs vary in undocumented ways  
- randomness, seeds, and tau-grids aren’t handled consistently  
- estimators are compared under shifting assumptions  

These issues make results hard to trust and even harder to compare.

The Constitution exists to prevent exactly this.  
It’s not a philosophical manifesto and not an attempt to imitate large standards bodies.  
It is simply the cleanest way to enforce **good scientific hygiene**.

### What the Constitution provides

- **A stable definition of truth**  
  so ATT and QST mean the same thing everywhere.

- **A fixed tau-grid**  
  so quantile effects stay comparable.

- **Clear rules for randomness and seeds**  
  so experiments are reproducible.

- **Constraints on DGP behaviour**  
  so synthetic data remains scientifically credible.

- **A separation between scientific rules and code**  
  so the implementation can evolve without changing the meaning.

## A note on the future

We hope that, over time, the community will pick up the framework — adding their own estimators, proposing new DGPs, and extending the benchmark in ways we didn’t originally imagine.

For that to work **without chaos**, everyone needs to operate under the same shared rules.  
The Constitution provides exactly that common foundation.

## In short

The Constitution is nothing grandiose — just a **practical safeguard** ensuring that:

> **CausalStress results remain clear, comparable, and fair — even as more people contribute.**

It keeps the science stable, trustworthy, and easy to build on.
