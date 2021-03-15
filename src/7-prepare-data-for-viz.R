#===============================================================================
# 2021-03-13-- sex gap e0
# Prepare data for specific plots
# Virgina Zarulli, vzarulli@sdu.dk
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))

# country names and codes
load("dat/ids.rda" %>% lp)

# decomp ------------------------------------------------------------------

load("dat/decomp.rda" %>% lp)


# order by the length of time series
df <- decomp %>% 
    right_join(ids, "country") %>% # add names
    group_by(country, age) %>% 
    mutate(nyrs = n()) %>% 
    ungroup() %>% 
    arrange(nyrs %>% desc, country, year, age) %>% 
    mutate(
        country = country %>% as_factor %>% fct_inorder(),
        name = name %>% as_factor %>% fct_inorder()
    ) %>% 
    # create facet positioning variables on a 7x5 canvas
    mutate(
        row = name %>% 
            lvls_revalue(
                new_levels = 1:7 %>% rep(5) %>% paste %>% head(33)
            ),
        column = name %>% 
            lvls_revalue(
                new_levels = 1:5 %>% rep(each = 7) %>% paste %>% head(33)
            )
    )

save(df, file = "dat/gap33cntrs.rda" %>% lp, compress = "xz")


# 6 age groups -- 1, 15, 40, 60, 80

load("dat/gap33cntrs.rda" %>% lp)

df6 <- df %>% 
    mutate(
        age_group = age %>% 
            cut(c(0, 1, 15, 40, 60, 80, 111), right = FALSE)
    ) %>% 
    group_by(country, name, row, column, year, age_group) %>% 
    summarise(
        ctb = ctb %>% sum(na.rm = T),
        ctb_rel = rel_ctb %>% sum(na.rm = T)
    ) %>% 
    ungroup() %>% 
    # relevel age_group factor
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-39", "40-59", "60-79", "80+"))
    )

save(df6, file = "dat/a6gap33cntrs.rda" %>% lp, compress = "xz")


# qx ----------------------------------------------------------------------

load("dat/lt33.rda" %>% lp)

# life table qx
df4qx <- lt33 %>%
    filter(
        country %>% is_in(c("SWE", "USA", "JPN", "RUS")),
        sex %>% is_in("b") %>% not,
        year == 2019 & country == "SWE" | 
            year == 2018 & country == "USA" |
            year == 2019 & country == "JPN"|
            year == 2014 & country == "RUS",
        age %>% is_less_than(96) & country != "RUS" |
            age %>% is_less_than(86) & country == "RUS"
    ) %>%
    transmute(sex, country, year, age, qx) %>% 
    mutate(
        country = country %>% as_factor() %>% 
            fct_recode(
                Sweden = "SWE",
                Japan = "JPN",
                Russia = "RUS"
            ) %>% 
            # re-order  to match the outer plot
            fct_relevel("USA", after = 0) %>% 
            fct_relevel("Sweden", after = 0)
    ) %>% 
    arrange(country, age)

save(df4qx, file = "dat/df4qx.rda" %>% lp)


# qx sex difference,scaled gap, and ratio
# prepare the death rates and diff df
qxdiff <- lt33 %>% 
    select(country, name, nyrs, sex, year, age, qx) %>% 
    filter(age %>% is_less_than(96)) %>% 
    mutate(
        age_group = age %>% 
            cut(c(0, 1, 15, 40, 60, 80, 111), right = FALSE) %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-39", "40-59", "60-79", "80+"))
    ) %>% 
    pivot_wider(names_from = sex, values_from = qx) %>% 
    mutate(gap = m - f,
           scaled = (m - f)/f,
           ratio = m/f)

save(qxdiff, file = "dat/qxdiff.rda" %>% lp)



# data for sens plateau 0.7 -----------------------------------------------

load("dat/decomp.rda" %>% lp)
load("dat/decomp_de_07.rda" %>% lp)

df_plateau <- decomp %>% 
    filter(country == "DEU") %>% 
    bind_rows(decomp_de_07, .id = "plateau") %>%
    mutate(
        plateau = plateau %>% as_factor() %>% 
            lvls_revalue(c("plateau=1", "plateau=0.7")),
        age_group = age %>% 
            cut(c(0, 1, 15, 40, 60, 80, 111), right = FALSE)
    ) %>% 
    group_by(plateau, country, year, age_group) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T),
              rel_ctb = rel_ctb %>% sum(na.rm = T) %>% multiply_by(100)) %>% 
    ungroup() %>% 
    # relevel age_group factor
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-39", "40-59", "60-79", "80+"))
    ) %>% 
    pivot_longer(contains("ctb"), names_to = "type") %>% 
    mutate(
        type = type %>% as_factor %>% 
            lvls_revalue(
                c("absolute, years", "relative, %")
            )
    )


save(df_plateau, file = "dat/df_plateau.rda" %>% lp)


# appendix seven -- sensitivity check for age boundary 50 vs 40 ------------

load("dat/gap33cntrs.rda" %>% lp)

df40 <- df %>% 
    mutate(age_group = age %>% 
               cut(c(0, 1, 15, 40, 60, 80, 111), right = FALSE)) %>% 
    group_by(country, name, row, column, year, age_group) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T),
              ctb_rel = rel_ctb %>% sum(na.rm = T)) %>% 
    ungroup() %>% 
    # relevel age_group factor
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-39", "40-59", "60-79", "80+"))
    )

df50 <- df %>% 
    mutate(age_group = age %>% 
               cut(c(0, 1, 15, 50, 60, 80, 111), right = FALSE)) %>% 
    group_by(country, name, row, column, year, age_group) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T),
              ctb_rel = rel_ctb %>% sum(na.rm = T)) %>% 
    ungroup() %>% 
    # relevel age_group factor
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-49", "50-59", "60-79", "80+"))
    )

save(df40, df50, file = "dat/df_40_50.rda" %>% lp)


# appendix eight -- decomp changes in the sex gap ----------------

load("dat/gap_decomp.rda" %>% lp)
load("dat/years_max_gap.rda" %>% lp)

# create labels with the period years
years_period <- years_max_gap %>% 
    pivot_wider(names_from = mark, values_from = year) %>% 
    transmute(
        country,
        early = paste0(first, "-\n", mid),
        late = paste0(mid, "-\n", last)
    ) %>% 
    pivot_longer(early:late, names_to = "period", values_to = "year_label")

# transform the dataset for plotting
df_gap_decomp <- gap_decomp %>% 
    pivot_longer(
        contains("ctb"), names_prefix = "ctb_", 
        names_to = "period", values_to = "ctb"
    ) %>% 
    mutate(
        age_group = age %>% 
            cut(c(0, 1, 15, 40, 60, 80, 111), right = FALSE)
    ) %>% 
    group_by(country, period, age_group) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T)) %>% 
    ungroup() %>% 
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-39", "40-59", "60-79", "80+"))
    ) %>% 
    # attach labels
    left_join(years_period) %>% 
    left_join(ids)

save(df_gap_decomp, file = "dat/df_gap_decomp.rda" %>% lp)
