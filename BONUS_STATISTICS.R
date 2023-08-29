points_share=Tabellone/sum(Tabellone)
HHCIB = length(teams)*sum(points_share^2)
Relative_entropy = sum(points_share*log(points_share))/log(1/length(teams))
p_one_block = posterior_k[1]
