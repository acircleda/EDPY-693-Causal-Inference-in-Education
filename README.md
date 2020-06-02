```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(knitr)
library(haven)
library(psych)
library(apaTables)
library(gt)
```

The following analyses come from the following textbooks related to causal inference:

  * Murnane, R. J., & Willett, J. B. (2010). *Methods matter: Improving causal inference in educational and social science research*. Oxford University Press.
  * Gertler, P. J., Martinez, S., Premand, P., Rawlings, L. B., & Vermeersch, C. M. (2016). *Impact evaluation in practice*. The World Bank.
  * Angrist, J. D., & Pischke, J. S. (2008). *Mostly harmless econometrics: An empiricist's companion*. Princeton University Press.
  
# Randomized Experiments

## New York Scholarship Program

The following example comes from *Methods matter*, chapter 4. The data was originally downloaded from the [UCLA Institute for Digital Research and Education](https://stats.idre.ucla.edu/stata/examples/methods-matter/chapter4/).

The data examines a subset of African-American students from the 1997 New York Scholarship Program, a lottery for school vouchers. 


**Variables**:

  * s_id: id number
  * voucher: recieved a voucher (1=yes, 0=no)
  * pre_ach: reading achievement score before getting a voucher
  * post_ach: reading achivemenet score at year 3
  
### Descriptive Statistics
```{r message=FALSE, warning=FALSE}
nysp_vouchers <- read_dta("data/methods_matter/ch4_nyvoucher.dta")

describe(nysp_vouchers) %>%
  mutate(across(is.numeric, round, 3)) %>%
  select(-mad, -min, -max, -range, -trimmed) %>%
  kable()
```

```{r message=FALSE, warning=FALSE}
#combined
rbind(nysp_vouchers %>%
  summarize(voucher = "combined",
            obs = n(),
            mean = mean(post_ach),
            "std. err." = sd(post_ach)/sqrt(n()),
            sd = sd(post_ach)),
  nysp_vouchers %>%
  group_by(voucher) %>%
  summarize(obs = n(),
            mean = mean(post_ach),
            "std. err." = sd(post_ach)/sqrt(n()),
            sd = sd(post_ach))) %>%
  kable()

```

!!! How to calculate CI for each group?
!!! How to get SE of the difference?

### T-Test

(Strategy 1, Table 4.1, pg. 49)

```{r, mm4_ttest}
mm4_t <- t.test(nysp_vouchers$post_ach ~ nysp_vouchers$voucher, var.equal = T)

broom::tidy(mm4_t) %>%
  rename("no voucher" = estimate1,
         "voucher" = estimate2) %>%
  kable()
```

### Simple Linear Regression 

(Strategy 2, Table 4.1, pg 49)

```{r, mm4_ols}
#model
mm4_model1 <- lm(post_ach ~ voucher, data=nysp_vouchers)

#table - attempt to use gt package for APA-like table
apa.reg.table(mm4_model1)[[3]] %>%
  gt() %>%
  tab_options(
    table.border.top.width = 2,
    table.border.top.color = "black",
    heading.border.bottom.width = 2,
    heading.border.bottom.color = "black",
    row_group.border.bottom.color = NULL,
    row_group.border.top.color = NULL,
    table_body.hlines.color = "white"
  )
```

#### Output for variances 

```{r}
apa.aov.table(mm4_model1)[[3]] %>% kable()
```

### Multiple Linear Regression 

(Strategy 3, Table 4.1, pg 49)

```{r, mm4_ols2}
mm4_model2 <- lm(post_ach ~ voucher + pre_ach, data = nysp_vouchers)
apa.reg.table(mm4_model2)[[3]] %>% kable()
```

#### Output for variances 

```{r}
apa.aov.table(mm4_model2)[[3]] %>% kable()
```

