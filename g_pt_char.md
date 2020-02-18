<style>
body{
  font-family: 'Oxygen', sans-serif;
  font-size: 16px;
  line-height: 24px;
}

h1,h2,h3,h4 {
  font-family: 'Raleway', sans-serif;
}

.container { width: 1000px; }
h3 {
  background-color: #D4DAEC;
  text-indent: 100px; 
}
h4 {
  text-indent: 100px;
}

g-table-intro h4 {
  text-indent: 0px;
}
</style>
Introduction
------------

This simulation is to demonstrate how Rmd can be used to practice
reproducible research.

-   Population: simulated subjects  
-   Characteristics analyzed:
    -   Sex  
    -   Age  
    -   Race group  
    -   Smoking history  
    -   High prevalent medical conditions (5)

Input datasets
--------------

-   input\_df.csv

<!-- -->

    rm(list=ls())

    options(stringsAsFactors = FALSE)

    my_spec <- cols(
      medrioid = col_double(),
      icdat = col_date(format = ""),
      network = col_character(),
      pt_chars = col_character(),
      value = col_character(),
      icdat_rev = col_date(format = ""),
      icdat_rev2 = col_date(format = ""),
      var_lbl = col_character()
    )

    adsl_care_long <- read_csv("/Users/tma/Desktop/g_demo/Rmd_demo/input_df.csv",
                               col_types = my_spec)

Visualization
-------------

    #customer functions------------------------------------------------------
    #my_date <- as.Date(c("2016-01-02", "2016-03-04", "2017-09-08"))

    single_year <- function(x){
          y <- if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
                              paste(month(x, label = TRUE), "\n", year(x)),
                              paste(month(x, label = TRUE)))

          return (y)
    }

    #x <- single_year(my_date)

### Sex

    #sex-----------------------------------------------------------------
    d_sex <- adsl_care_long %>%
          dplyr::filter(pt_chars == "sex") %>%
          # dplyr::mutate(icdat_rev2 = as.Date(paste0(lubridate::year(icdat), "-", lubridate::month(icdat), "-", "1"))) %>%
          dplyr::add_count(pt_chars, network, icdat_rev2) %>%
          dplyr::rename(N = n) %>% 
          dplyr::add_count(pt_chars, network, icdat_rev2, value ) %>%
          dplyr::distinct(icdat_rev2, network, pt_chars, network, value, N, n ) %>%
          dplyr::mutate(pct = n / N)

    p_sex <- ggplot(data=d_sex, mapping = (aes(x=icdat_rev2, y=pct))) +
          geom_point(aes(size=n, color=value)) +
          #    scale_x_date(date_breaks = "1 month", date_labels = "%y-%b") +
          scale_fill_brewer(palette="Set3") +
          facet_wrap(~ network, nrow = 10 ) +
          theme(text = element_text(size=11)) +
          scale_size_continuous (breaks = c(20, 100, 200)) +
          theme(legend.position="bottom", legend.title=element_blank())


    ##fix labels for dates on X axis==
    p_sex <- p_sex + scale_x_date(date_breaks = "1 months",
                                  labels = single_year) +
          geom_vline(xintercept = as.Date(c("2017-01-01", "2018-01-01")), colour="grey60")

    p_sex

<img src="g_pt_char_files/figure-markdown_strict/p_sex-1.png" width="100%" />

### Age

    #age ------------------------------------------------------------------------
    d_age <- adsl_care_long %>%
          dplyr::filter(pt_chars == "age")


    p_age <- ggplot(data=d_age, mapping = (aes(x=icdat_rev2, y=as.numeric(value), group = icdat_rev2)), na.rm=TRUE) +
          geom_boxplot(color = "#009999") +
          scale_fill_brewer(palette="Set3") +
          facet_wrap (~ network, ncol=1) +
          scale_y_continuous(name="Age", limits=c(10, 100), breaks = seq(0, 100, 10), minor_breaks = NULL) +
          theme(legend.position="bottom")


    p_age <- p_age + scale_x_date(date_breaks = "1 months",
                         labels = single_year) +
          geom_vline(xintercept = as.Date(c("2017-01-01", "2018-01-01")), colour="grey60") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), legend.title=element_blank())

    p_age

<img src="g_pt_char_files/figure-markdown_strict/p_age-1.png" width="100%" />

### Race groups

    #race group-----------------------------------------------------------------
    d_race <- adsl_care_long %>%
          dplyr::filter(pt_chars == "racecat", value != "blank") %>%
          dplyr::add_count(pt_chars, network, icdat_rev2) %>%
          dplyr::rename(N=n) %>% 
          dplyr::add_count(pt_chars, network, icdat_rev2, value ) %>%
          dplyr::distinct(icdat_rev2, network, pt_chars, network, value, N, n) %>%
          dplyr::mutate(pct = n / N)

    p_race <- ggplot(data=d_race, mapping = (aes(x=icdat_rev2, y=pct, color=value))) +
          geom_line(aes(color=value)) +
          geom_point(aes(size=n, color=value)) +
          scale_fill_brewer(palette="Accent") +
          scale_color_discrete(breaks=c("White, non-Hispanic",
                                        "Black, non-Hispanic",
                                        "Hispanic",
                                        "Other/unknown",
                                        "Asian, Native Hawaiian or Pacific Islander",
                                        "American Indian or Alaska Native"
                                        )) +
          guides(size = guide_legend(order=1),
                 color = guide_legend(order=2)) +
          facet_wrap(~ network, ncol=1, labeller = label_both )

    p_race <- p_race + scale_x_date(date_breaks = "1 months",
                                  labels = single_year) +
          geom_vline(xintercept = as.Date(c("2017-01-01", "2018-01-01")), colour="grey60") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), legend.position="bottom", legend.title=element_blank(),
                legend.text=element_text(size=10))

    p_race

<img src="g_pt_char_files/figure-markdown_strict/p_race-1.png" width="100%" />

### Smoking history

    #smoke history-----------------------------------------------------------------
    d_smk <- adsl_care_long %>%
          dplyr::filter(pt_chars == "smkcat", value != "blank") %>%
          dplyr::add_count(pt_chars, network, icdat_rev2) %>%
          dplyr::rename(N=n) %>% 
          dplyr::add_count(pt_chars, network, icdat_rev2, value ) %>%
          dplyr::distinct(icdat_rev2, network, pt_chars, network, value, N, n) %>%
          dplyr::mutate(pct = n / N)

    p_smk <- ggplot(data=d_smk, mapping = (aes(x=icdat_rev2, y=pct, color=value))) +
          geom_line(aes(color=value)) +
          geom_point(aes(size=n, color=value)) +
          scale_fill_brewer(palette="Set3") +
          scale_color_discrete(breaks=c("Non-smoker",
                                        "Former smoker",
                                        "Current smoker",
                                        "Missing"

          )) +
          facet_wrap(~ network, ncol=1, labeller = label_both )

    p_smk <- p_smk + scale_x_date(date_breaks = "1 months",
                                    labels = single_year) +
          geom_vline(xintercept = as.Date(c("2017-01-01", "2018-01-01")), colour="grey60") +
          theme_bw() +
          theme(panel.grid.minor = element_blank(), legend.position="bottom", legend.title=element_blank(),
                legend.text=element_text(size=10))

    p_smk

<img src="g_pt_char_files/figure-markdown_strict/p_smk-1.png" width="100%" />

### Medical history (most relevant ones)

    #medical history (non-malignant medical conditions)----------------------------
    d_mh <- adsl_care_long %>%
          dplyr::filter(grepl("crmce..", pt_chars) == T, value != "blank")

    ##select top 5 medical conditions ==
    d_mh_yes <- d_mh %>%
          dplyr::filter(value == "Yes")

    cnt <- d_mh_yes %>%
          dplyr::count(pt_chars) %>%
          dplyr::mutate(rank = rank(n * -1)) %>%
          dplyr::filter(rank<= 5) %>%
          dplyr::select(-n)

    d_mh_top <- left_join(cnt, d_mh, by="pt_chars") %>%
          dplyr::add_count(pt_chars, var_lbl, rank, network, icdat_rev2) %>%
          dplyr::rename(N=n) %>% 
          dplyr::add_count(pt_chars, var_lbl, rank, network, icdat_rev2, value ) %>%
          dplyr::distinct(icdat_rev2, network, pt_chars, var_lbl, rank, network, value, N, n) %>%
          dplyr::arrange(rank, icdat_rev2) %>%
          dplyr::mutate(pct = n / N)

    one_plot <- function(one_char){
    #      one_char = "crmce01"
          df <- d_mh_top %>% dplyr::filter(pt_chars==one_char)
          one_char_lbl = unique(df$var_lbl)
          p_mh <- ggplot(data=df, mapping = (aes(x=icdat_rev2, y=pct, color=value))) +
                geom_point(aes(size=n, color=value)) +
                scale_fill_brewer(palette="Set3") +
                scale_color_discrete(breaks=c("Yes",
                                              "No/not sure",
                                              "NA before protocol E"

                )) +
                facet_wrap (~ network, ncol=1, labeller = label_both ) +
                ggtitle(one_char_lbl)

          p_mh <- p_mh + scale_x_date(date_breaks = "1 months",
                                        labels = single_year) +
                geom_vline(xintercept = as.Date(c("2017-01-01", "2018-01-01")), colour="grey60") +
                theme_bw() +
                theme(panel.grid.minor = element_blank(), legend.position="bottom", legend.title=element_blank(),
                legend.text=element_text(size=10))

    }

    mh_chars <- unique(d_mh_top$pt_chars)
    p_mhs <- map(mh_chars, one_plot)

    p_mhs[[1]]

<img src="g_pt_char_files/figure-markdown_strict/p_mh-1.png" width="100%" />

    p_mhs[[2]]

<img src="g_pt_char_files/figure-markdown_strict/p_mh-2.png" width="100%" />

    p_mhs[[3]]

<img src="g_pt_char_files/figure-markdown_strict/p_mh-3.png" width="100%" />

    p_mhs[[4]]

<img src="g_pt_char_files/figure-markdown_strict/p_mh-4.png" width="100%" />

    p_mhs[[5]]

<img src="g_pt_char_files/figure-markdown_strict/p_mh-5.png" width="100%" />
