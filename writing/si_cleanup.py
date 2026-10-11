# Remove the "Its function is ..." tics from the SI text and smooth what remains.
import re

REPLACEMENTS = [
 ("This table's function is scope documentation. It tells the reader how the archive evolves over time and shows that the paper's outcomes are observed across a large and expanding corpus rather than a thin event-study slice.",
  "The table shows how the archive grows over time; the outcomes are observed across a large and expanding corpus rather than a thin window around the cutoffs."),
 ("Its function is complementary to the year table: it documents the breadth of spatial coverage and the degree of cross-city variation that later heterogeneity and robustness checks draw on.",
  "It documents the breadth of spatial coverage and the cross-city variation that the heterogeneity and robustness checks draw on."),
 ("The figure's function is descriptive context. It makes clear that the corpus varies not only in how much governments post, but also in how much attention their audiences appear to pay.",
  "The corpus varies not only in how much governments post but also in how much attention their audiences pay."),
 ("Its function is mechanism robustness. The table shows that the 2020 decomposition is not a fragile byproduct of one preferred bandwidth or polynomial choice: the same sign pattern survives every standard RDD perturbation.",
  "The 2020 decomposition does not depend on one bandwidth or polynomial choice; the same sign pattern survives every standard perturbation."),
 ("Its function is participation-margin decomposition. The table separates the decision to leave any trace from the amount of engagement conditional on participation, and it shows that the 2020 redesign mainly widened participation in low-visibility approval without generating a parallel increase in sharing.",
  "The table separates the decision to leave any trace from the amount of engagement among those who do. The 2020 redesign mainly widened participation in low-visibility approval and did not produce a parallel increase in sharing."),
 ("The function of this table is to show that the main 2018 result is not easy to reproduce at arbitrary nearby dates. That matters because it pushes against purely seasonal or slowly evolving explanations.",
  "The main 2018 result is not easy to reproduce at arbitrary nearby dates, which weighs against seasonal or slowly evolving explanations."),
 ("Its function is interpretive cleanup. The table distinguishes a modest wording effect from the much larger redesign effects that altered the visibility structure of engagement.",
  "The table distinguishes a modest wording effect from the much larger effects of the two redesigns."),
 ("The function of this contrast table is conceptual. It shows that not every WeChat interface reform belongs in the same theoretical bucket: discovery changes look different from approval-visibility changes.",
  "Not every WeChat interface change belongs in the same category: a change in how articles are discovered looks different from a change in who can see approval."),
 ("Its function is weighting robustness. The table addresses the concern that article-level estimates might be driven disproportionately by a small number of high-volume accounts.",
  "This addresses the concern that article-level estimates are driven by a small number of high-volume accounts."),
 ("Its function is alternative-explanation control. The table directly addresses the possibility that the 2020 result is really a pandemic-composition artifact rather than an interface effect.",
  "This addresses the possibility that the 2020 result reflects the pandemic's effect on content rather than the interface."),
 ("The function of the extended balance table is diagnostic transparency. Rather than hiding marginal composition changes, it shows exactly where they appear and why the paper leans on a bundle of robustness checks rather than a single balance screen.",
  "Rather than omit the marginal composition changes, the table shows where they appear, which is why the paper relies on several robustness checks rather than a single balance test."),
 ("Each panel has a distinct job: the 2018 share panel functions as a placebo visual, the 2020 one-click panel shows the aggregate recovery, the 2020 like panel isolates the restored low-visibility channel, and the 2020 share panel shows that broader forwarding behavior does not move in parallel.",
  "The 2018 share panel is a placebo, the 2020 one-click panel shows the aggregate recovery, the 2020 like panel isolates the restored low-visibility channel, and the 2020 share panel shows that forwarding does not move in parallel."),
 ("Its function is granularity. The table shows that the family-level gradient is not an artifact of broad category aggregation but survives when the analysis moves to the underlying ten-category taxonomy.",
  "The family-level gradient survives when the analysis moves to the underlying ten-category taxonomy."),
 ("The figure's function is coverage. It shows that the 2018 visibility penalty is not confined to one substantive domain, even though the later 2020 reallocation differs by political risk.",
  "The 2018 visibility penalty is not confined to one substantive domain, even though the 2020 reallocation differs by political content."),
 ("Its function is bridge-building between aggregate and decomposed evidence. The figure shows that total one-click recovery is broad, while the main-text family unbundling figure explains why that broad recovery does not imply identical underlying expressive behavior across families.",
  "Total one-click recovery is broad; the family-level unbundling figure in the main text shows why that broad recovery does not imply the same behavior across families."),
 ("The function of this table is prominence heterogeneity. It asks whether interface effects depend on article placement and therefore on the amount of ambient attention an article already receives.",
  "The table asks whether the interface effects depend on article placement, and so on how much attention an article already receives."),
 ("Its function is contextual heterogeneity. The table probes whether visibility costs are stronger in communicative environments where social exposure is plausibly denser.",
  "The table asks whether visibility costs are stronger where social exposure is plausibly denser."),
 ("The function of this table is measurement robustness. It shows that the main results do not depend on the most ambiguous title classifications.",
  "The main results do not depend on the most ambiguous title classifications."),
 ("Their function is visual transparency. Instead of relying only on local-polynomial summaries, the figures show the noisier day-level data that the RDD is summarizing.",
  "The figures show the noisier day-level data that the local-polynomial fits summarize."),
 ("Its function is unit-level context: the table clarifies what kinds of accounts populate the archive before article-level estimates aggregate across them.",
  "The table shows what kinds of accounts populate the archive."),
 ("Its function is taxonomy resolution. The table lets readers inspect the fine-grained categories that underlie the paper's broader family groupings.",
  "The table lets readers inspect the fine-grained categories behind the family groupings."),
 ("Its function is to combine the paper's two main descriptive dimensions: margin of engagement and content type. It shows how participation versus intensity varies not only across regimes but also across substantive families.",
  "It shows how participation and intensity vary across regimes and across families."),
 ("The figure's function is dynamic corroboration. It shows in time-series form that the participation margin moves in the same directions highlighted by the descriptive regime table.",
  "In time-series form, the participation margin moves in the same directions as in the regime table."),
]

def clean(text):
    for old, new in REPLACEMENTS:
        if old in text:
            text = text.replace(old, new)
        else:
            print('WARNING: SI phrase not found:', old[:60])
    # generic safety net for any remaining tics
    text = re.sub(r"(Its|Their|The figure's|The table's|This table's) function is [^.]*\.\s*", "", text)
    return text
