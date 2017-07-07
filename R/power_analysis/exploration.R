source("util.R")

edit_rate_baseline = 0.319
survival_rate_baseline = 0.146
edit_n = seq(500, 200000, length.out=1000)
survival_n = seq(500, 50000, length.out=1000)

dt = rbind(
  data.table(
    group="edit rate",
    baseline=edit_rate_baseline,
    change=as.character(0.01),
    new_rate=edit_rate_baseline-0.01,
    n=edit_n
  ),
  data.table(
    group="edit rate",
    baseline=edit_rate_baseline,
    change=as.character(0.02),
    new_rate=edit_rate_baseline-0.02,
    n=edit_n
  ),
  data.table(
    group="edit rate",
    baseline=edit_rate_baseline,
    change=as.character(0.03),
    new_rate=edit_rate_baseline-0.03,
    n=edit_n
  ),
  data.table(
    group="survival rate",
    baseline=survival_rate_baseline,
    change=as.character(0.01),
    new_rate=survival_rate_baseline-0.01,
    n=survival_n
  ),
  data.table(
    group="survival rate",
    baseline=survival_rate_baseline,
    change=as.character(0.02),
    new_rate=survival_rate_baseline-0.02,
    n=survival_n
  ),
  data.table(
    group="survival rate",
    baseline=survival_rate_baseline,
    change=as.character(0.03),
    new_rate=survival_rate_baseline-0.03,
    n=survival_n
  )
)

dt$p.value = dt[,
  list(p.value=prop.test(c(new_rate*n, baseline*n), c(n, n))$p.value),
  by=1:nrow(dt)]$p.value

svg("power_analysis/edit_rate_change.enwiki.april_2017.svg",
    height=5, width=7)
ggplot(dt[group=="edit rate",], aes(x=n, y=p.value, group=change)) +
theme_bw() +
geom_hline(yintercept=0.05, linetype=2) +
geom_vline(xintercept=c(34871, 34871*2), linetype=2, color="gray") +
geom_line(aes(color=change))
dev.off()

svg("power_analysis/survival_rate_change.enwiki.april_2017.svg",
    height=5, width=7)
ggplot(dt[group=="survival rate",], aes(x=n, y=p.value, group=change)) +
theme_bw() +
geom_hline(yintercept=0.05, linetype=2) +
geom_vline(xintercept=c(11111, 11111*2), linetype=2, color="gray") +
geom_line(aes(color=change))
dev.off()
