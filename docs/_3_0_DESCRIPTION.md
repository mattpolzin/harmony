# DESCRIPTION
Harmony is a small tool that helps teams keep GitHub reviews running smoothly.
It takes the work out of picking someone from a pool of developers to review a
new PR. Harmony does this by heuristically determining who on a particular
GitHub Team has the least current/recent review workload.

Harmony offers a heuristic for PR review requests that is different than
GitHub's round robin or weighted algorithms, but Harmony can also work well even
if your team uses GitHub's automatic PR review requests ([see below](#deferring-to-github)).

