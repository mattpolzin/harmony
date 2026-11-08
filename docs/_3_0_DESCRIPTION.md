# DESCRIPTION
Harmony is a tool that helps teams keep GitHub reviews running smoothly.
It takes the work out of picking someone from a pool of developers to review a
new PR. Harmony does this by heuristically determining who on a particular
GitHub Team has the least current/recent review workload.

Harmony offers a heuristic for PR review requests that is different than
GitHub's round robin or weighted algorithms, but Harmony can also work well even
if your team uses GitHub's automatic PR review requests ([see below](#deferring-to-github)).

Harmony has grown over time to also offer shortcuts that make working with pull
requests and their tickets and branches easier (for one particular personal take
on what "easier" means). If the opinionated decisions made in building this tool
end up working for you (e.g. using slugs in branch names to tie branches to
issues, using comments in issues to determine if downstream PRs should close
them, etc.) then you may find that harmony automates much of the connective
tissue for you without requiring as much manual work (or relying as much on
GenAI skills).

