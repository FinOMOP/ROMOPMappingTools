.summaryToSummaryTable <- function(summary) {
    summary |> checkmate::assert_tibble()

    # Define color mapping for test outcomes
    outcome_color_map <- c(
        "NA" = "#D3D3D3", # light gray
        "N" = "#808080", # medium-dark gray
        "A" = "#CD853F", # orange-brown (peru)
        "AA" = "#DEB887", # light orange/beige (burlywood)
        "L" = "#9370DB", # medium purple
        "LL" = "#DDA0DD", # light purple (plum)
        "H" = "#DC143C", # red (crimson)
        "HH" = "#8B0000" # dark red/maroon
    )

    valueSource_color_map <- c(
        "Missing" = "#D3D3D3",
        "Extracted" = "#808080",
        "Source" = "#0000FF"
    )

    toplot <- summary |>
        dplyr::mutate(
            tmpMU = dplyr::if_else(is.na(MEASUREMENT_UNIT), "", MEASUREMENT_UNIT),
            tmpMUHarmonized = dplyr::if_else(is.na(MEASUREMENT_UNIT_HARMONIZED), "", MEASUREMENT_UNIT_HARMONIZED),
            valueUnitChange = paste0(
                dplyr::if_else(tmpMU!=tmpMUHarmonized, paste0(tmpMU, " -> ", tmpMUHarmonized), ""),
                "<br>",
                dplyr::if_else(is.na(CONVERSION_FACTOR), "", paste0("* ", CONVERSION_FACTOR)))
        ) |>
        dplyr::select(-tmpMU, -tmpMUHarmonized) |>
        dplyr::transmute(
            conceptId = OMOP_CONCEPT_ID,
            conceptName = concept_name,
            omopQuantity = omopQuantity,
            testId = paste0(TEST_NAME, " [", dplyr::if_else(is.na(MEASUREMENT_UNIT), "", MEASUREMENT_UNIT), "]"),
            nPeople = n_subjects,
            nRecords = n_records,
            dValuesSource = distribution_values,
            dOutcomes = distribution_outcomes,
            dMeasurementValue = purrr::map_chr(decile_MEASUREMENT_VALUE, .decileText),
            ksTest = ksTest,
            valueUnitChange = valueUnitChange,
            dMeasurementValueHarmonized = purrr::map_chr(decile_MEASUREMENT_VALUE_HARMONIZED, .decileText),
            ksTestHarmonized = ksTestHarmonized,
            message = message
        ) |>
        dplyr::arrange(dplyr::desc(nRecords))


    plot <- toplot |>
        reactable::reactable(
            filterable = TRUE,
            sortable = TRUE,
            searchable = TRUE,
            defaultPageSize = 15,
            showPageSizeOptions = TRUE,
            resizable = TRUE,
            columns = list(
                conceptId = reactable::colDef(
                    name = "OMOP Concept ID",
                    maxWidth = 100
                ),
                conceptName = reactable::colDef(
                    name = "Omop Name"
                ),
                omopQuantity = reactable::colDef(
                    name = "Omop Quantity",
                    maxWidth = 150
                ),
                testId = reactable::colDef(
                    name = "TestCode [Unit]",
                    maxWidth = 150
                ),
                nPeople = reactable::colDef(
                    name = "N people",
                    maxWidth = 80
                ),
                nRecords = reactable::colDef(
                    name = "N records",
                    maxWidth = 80
                ),
                dValuesSource = reactable::colDef(
                    name = "Value source",
                    html = TRUE,
                    cell = function(value) {
                        .proportionBarHTML(value, color_map = valueSource_color_map)
                    },
                    minWidth = 50,
                    maxWidth = 100
                ),
                dOutcomes = reactable::colDef(
                    name = "Test outcome",
                    html = TRUE,
                    cell = function(value) {
                        .proportionBarHTML(value, color_map = outcome_color_map)
                    },
                    minWidth = 50,
                    maxWidth = 100
                ),
                dMeasurementValue = reactable::colDef(
                    name = "Decile of measurement value",
                    minWidth = 150
                ),
                ksTest = reactable::colDef(
                    name = "KS(mpl)",
                    html = TRUE,
                    minWidth = 50,
                    cell = function(value) {
                        .renderKSTest(value)
                    }
                ),
                valueUnitChange = reactable::colDef(
                    name = "Value unit change",
                    html = TRUE,
                    minWidth = 50
                ),
                dMeasurementValueHarmonized = reactable::colDef(
                    name = "Decile of harmonized measurement value",
                    minWidth = 150
                ),
                ksTestHarmonized = reactable::colDef(
                    name = "KS(mpl) harmonized",
                    html = TRUE,
                    minWidth = 50,
                    cell = function(value) {
                        .renderKSTest(value)
                    }
                ),
                message = reactable::colDef(
                    name = "Message",
                    minWidth = 150
                )
            )
        )


    plot_with_tooltips <- htmltools::tagList(
        .proportionBarTooltipStyle,
        .proportionBarTooltipScript,
        plot
    )

    pathToHtmlFile <- tempfile(fileext = ".html")
    plot_with_tooltips |> htmltools::save_html(file = pathToHtmlFile)
    utils::browseURL(pathToHtmlFile)
}

.renderKSTest <- function(ksTest) {
    if (is.null(ksTest)) {
        return("")
    }
    ks <- ksTest  |> dplyr::pull(KS) |> round(digits = 2)
    pValue <- ksTest  |> dplyr::pull(pValue) 
    mplog10pValue <- -log10(pValue) |>
     round(digits = 2)
    bg_color <- ""
    if (!is.null(ks)) {
        if (ks > 0.5) {
            bg_color <- "#ffcccc" # red
        } else if (ks > 0.2) {
            bg_color <- "#ffffcc" # yellow
        } else {
            bg_color <- "#ccffcc" # green
        }
    }
    htmltools::HTML(
        htmltools::HTML(
            paste0(
                '<span style="padding:2px 4px; border-radius:4px;background-color: ', bg_color, ';">',
                "", ks, "<br>(", mplog10pValue, ")",
                "</span>"
            )
        )
    )
}

.decileText <- function(decileTibble) {
    if (is.null(decileTibble) || (tibble::is_tibble(decileTibble) && nrow(decileTibble) == 0)) {
        return("")
    }

    values <- decileTibble |>
        dplyr::arrange(decile) |>
        dplyr::pull(value) |>
        round(digits = 2) |>
        paste0(collapse = ", ")

    paste0("[ ", values, " ]")
}

.proportionBarHTML <- function(proportionsTibble, color_map) {
    # Handle NULL or empty tibble
    if (is.null(proportionsTibble) || (tibble::is_tibble(proportionsTibble) && nrow(proportionsTibble) == 0)) {
        return(htmltools::HTML('<div style="height: 28px;"></div>'))
    }

    # Validate tibble has required columns
    if (!all(c("value", "n") %in% names(proportionsTibble))) {
        stop("Tibble must have columns 'value' and 'n'")
    }

    # Validate color_map is provided
    if (missing(color_map) || is.null(color_map)) {
        stop("color_map must be provided")
    }

    # Define sort order from color map keys
    outcome_order <- names(color_map)

    # Sort outcomes according to predefined order
    proportionsTibble <- proportionsTibble |>
        dplyr::mutate(
            sort_order = dplyr::case_when(
                value %in% outcome_order ~ match(value, outcome_order),
                TRUE ~ Inf
            )
        ) |>
        dplyr::arrange(sort_order) |>
        dplyr::select(-sort_order)

    # Extract category names and counts (now sorted)
    category_names <- proportionsTibble$value
    counts <- proportionsTibble$n

    # Calculate total and proportions
    total_count <- sum(counts, na.rm = TRUE)

    # Handle zero sum
    if (total_count == 0) {
        total_count <- 1
    }

    props <- counts / total_count
    n_categories <- length(category_names)

    # Assign colors based on category names from color_map
    colors <- character(n_categories)
    for (i in seq_len(n_categories)) {
        category_name <- category_names[i]
        if (category_name %in% names(color_map)) {
            colors[i] <- color_map[[category_name]]
        } else {
            # Fallback color for unknown categories
            colors[i] <- "#CCCCCC"
        }
    }

    # Build gradient stops for the bar
    bar_stops <- character(n_categories)
    cumulative_pct <- 0
    for (i in seq_len(n_categories)) {
        pct <- props[i] * 100
        bar_stops[i] <- paste0(colors[i], " ", cumulative_pct, "%, ", colors[i], " ", cumulative_pct + pct, "%")
        cumulative_pct <- cumulative_pct + pct
    }
    bar_gradient <- paste(bar_stops, collapse = ", ")

    # Prepare tooltip data with outcome, percentage, and count
    tooltip_data <- purrr::map_chr(
        seq_len(n_categories),
        function(i) {
            paste0(
                '{"outcome":"', category_names[i], '","pRecords":', round(props[i] * 100, 2), ',"nRecords":', counts[i], ',"color":"', colors[i], '"}'
            )
        }
    )
    tooltip_data_json <- paste0("[", paste(tooltip_data, collapse = ","), "]")

    # Create HTML for one bar with tooltip data
    # Escape quotes in JSON for HTML attribute
    tooltip_data_json_escaped <- gsub('"', "&quot;", tooltip_data_json, fixed = TRUE)
    htmltools::HTML(paste0(
        '<div class="proportion-bar-tooltip" data-tooltip-table="', tooltip_data_json_escaped, '" style="display: flex; flex-direction: column; gap: 4px; width: 100%; overflow: visible; position: relative;">',
        '<div style="height: 12px; width: 100%; background: linear-gradient(to right, ', bar_gradient, '); border-radius: 2px; position: relative; overflow: visible;"></div>',
        "</div>"
    ))
}


# CSS and JavaScript for tooltips that escape cell boundaries
.proportionBarTooltipStyle <- htmltools::tags$style(
    ".proportion-bar-tooltip {
        cursor: pointer;
    }
    .proportion-tooltip {
        position: fixed;
        padding: 6px 10px;
        background-color: rgba(0, 0, 0, 0.9);
        color: white;
        border-radius: 4px;
        font-size: 12px;
        z-index: 99999;
        pointer-events: none;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
        display: none;
    }
    .proportion-tooltip.above::before {
        content: '';
        position: absolute;
        top: -5px;
        left: 50%;
        transform: translateX(-50%);
        width: 0;
        height: 0;
        border: 5px solid transparent;
        border-bottom-color: rgba(0, 0, 0, 0.9);
    }
    .proportion-tooltip.below::before {
        content: '';
        position: absolute;
        bottom: -5px;
        left: 50%;
        transform: translateX(-50%);
        width: 0;
        height: 0;
        border: 5px solid transparent;
        border-top-color: rgba(0, 0, 0, 0.9);
    }"
)

# JavaScript to handle tooltip positioning
.proportionBarTooltipScript <- htmltools::tags$script(
    htmltools::HTML('
    (function() {
        let tooltip = null;

        function createTooltip() {
            if (!tooltip) {
                tooltip = document.createElement("div");
                tooltip.className = "proportion-tooltip";
                document.body.appendChild(tooltip);
            }
            return tooltip;
        }

        function showSimpleTableTooltip(element, simpleData) {
            const tip = createTooltip();

            // Parse JSON data
            let data;
            try {
                data = JSON.parse(simpleData);
            } catch (e) {
                return;
            }

            // Create HTML table with 2 columns
            let tableHTML = "<table style=\\"border-collapse: collapse; width: 100%;\\"><thead><tr><th style=\\"padding: 4px 8px; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.3);\\">% Cases</th><th style=\\"padding: 4px 8px; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.3);\\">% Controls</th></tr></thead><tbody><tr><td style=\\"padding: 4px 8px; text-align: right;\\">" + data.case + "%</td><td style=\\"padding: 4px 8px; text-align: right;\\">" + data.control + "%</td></tr></tbody></table>";

            tip.innerHTML = tableHTML;
            tip.style.display = "block";

            positionTooltip(element, tip);
        }

        function showTableTooltip(element, tableData) {
            const tip = createTooltip();

            // Parse JSON data
            let data;
            try {
                data = JSON.parse(tableData);
            } catch (e) {
                return;
            }

            // Create HTML table with 3 columns: Outcome, pRecords, and nRecords
            let tableHTML = "<table style=\\"border-collapse: collapse; width: 100%;\\"><thead><tr><th style=\\"padding: 4px 8px; text-align: left; border-bottom: 1px solid rgba(255,255,255,0.3);\\">Outcome</th><th style=\\"padding: 4px 8px; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.3);\\">pRecords</th><th style=\\"padding: 4px 8px; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.3);\\">nRecords</th></tr></thead><tbody>";

            for (let i = 0; i < data.length; i++) {
                const outcomeColor = data[i].color || "#ffffff";
                tableHTML += "<tr><td style=\\"padding: 4px 8px; color: " + outcomeColor + ";\\">" + data[i].outcome + "</td><td style=\\"padding: 4px 8px; text-align: right;\\">" + data[i].pRecords + "%</td><td style=\\"padding: 4px 8px; text-align: right;\\">" + data[i].nRecords + "</td></tr>";
            }

            tableHTML += "</tbody></table>";
            tip.innerHTML = tableHTML;
            tip.style.display = "block";

            positionTooltip(element, tip);
        }

        function showBoxplotTooltip(element, boxplotData) {
            const tip = createTooltip();

            // Parse JSON data
            let data;
            try {
                data = JSON.parse(boxplotData);
            } catch (e) {
                return;
            }

            // Create HTML table with measure, Cases, Controls columns
            let tableHTML = "<table style=\\"border-collapse: collapse; width: 100%;\\"><thead><tr><th style=\\"padding: 4px 8px; text-align: left; border-bottom: 1px solid rgba(255,255,255,0.3);\\">Measure</th><th style=\\"padding: 4px 8px; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.3);\\">Cases</th><th style=\\"padding: 4px 8px; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.3);\\">Controls</th></tr></thead><tbody>";

            for (let i = 0; i < data.length; i++) {
                tableHTML += "<tr><td style=\\"padding: 4px 8px;\\">" + data[i].measure + "</td><td style=\\"padding: 4px 8px; text-align: right;\\">" + data[i].case + "</td><td style=\\"padding: 4px 8px; text-align: right;\\">" + data[i].control + "</td></tr>";
            }

            tableHTML += "</tbody></table>";
            tip.innerHTML = tableHTML;
            tip.style.display = "block";

            positionTooltip(element, tip);
        }

        function positionTooltip(element, tip) {
            const rect = element.getBoundingClientRect();
            const tipRect = tip.getBoundingClientRect();

            // Position above the bar, centered horizontally
            let top = rect.top - tipRect.height - 8;
            let left = rect.left + (rect.width / 2) - (tipRect.width / 2);

            // Adjust if tooltip would go off screen
            if (top < 0) {
                // Position below instead
                top = rect.bottom + 8;
                tip.classList.remove("above");
                tip.classList.add("below");
            } else {
                tip.classList.remove("below");
                tip.classList.add("above");
            }

            // Adjust horizontal position if off screen
            if (left < 0) {
                left = 8;
            } else if (left + tipRect.width > window.innerWidth) {
                left = window.innerWidth - tipRect.width - 8;
            }

            tip.style.top = top + "px";
            tip.style.left = left + "px";
        }

        function hideTooltip() {
            if (tooltip) {
                tooltip.style.display = "none";
            }
        }

        // Use event delegation for better performance
        document.addEventListener("mouseover", function(e) {
            const target = e.target.closest(".proportion-bar-tooltip");
            if (target) {
                const boxplotData = target.getAttribute("data-tooltip-boxplot");
                if (boxplotData !== null) {
                    showBoxplotTooltip(target, boxplotData);
                } else {
                    const simpleData = target.getAttribute("data-tooltip-simple");
                    if (simpleData !== null) {
                        showSimpleTableTooltip(target, simpleData);
                    } else {
                        const tableData = target.getAttribute("data-tooltip-table");
                        if (tableData !== null) {
                            showTableTooltip(target, tableData);
                        }
                    }
                }
            }
        });

        document.addEventListener("mouseout", function(e) {
            const target = e.target.closest(".proportion-bar-tooltip");
            if (target) {
                hideTooltip();
            }
        });

        document.addEventListener("mousemove", function(e) {
            const target = e.target.closest(".proportion-bar-tooltip");
            if (target && tooltip && tooltip.style.display === "block") {
                const boxplotData = target.getAttribute("data-tooltip-boxplot");
                if (boxplotData !== null) {
                    showBoxplotTooltip(target, boxplotData);
                } else {
                    const simpleData = target.getAttribute("data-tooltip-simple");
                    if (simpleData !== null) {
                        showSimpleTableTooltip(target, simpleData);
                    } else {
                        const tableData = target.getAttribute("data-tooltip-table");
                        if (tableData !== null) {
                            showTableTooltip(target, tableData);
                        }
                    }
                }
            }
        });
    })();
    ')
)


.calcualteKStest <- function(summary) {
    # find reference distribution
    referenceDistribution <- summary |>
        dplyr::filter(!is.na(MEASUREMENT_UNIT)) |>
        dplyr::arrange(dplyr::desc(n_records)) |>
        dplyr::distinct(OMOP_CONCEPT_ID, .keep_all = TRUE) |>
        dplyr::transmute(
            OMOP_CONCEPT_ID,
            n_values_reference = n_values,
            decile_MEASUREMENT_VALUE_reference = decile_MEASUREMENT_VALUE,
            decile_MEASUREMENT_VALUE_HARMONIZED_reference = decile_MEASUREMENT_VALUE_HARMONIZED,
            MEASUREMENT_UNIT_reference = MEASUREMENT_UNIT
        )

    # calculate KS test for each OMOP_CONCEPT_ID
    summary <- summary |>
        dplyr::left_join(referenceDistribution, by = "OMOP_CONCEPT_ID") |> 
        dplyr::mutate(
            n_values = n_values,
            ksTest = purrr::pmap(
                list(
                    decile_MEASUREMENT_VALUE, decile_MEASUREMENT_VALUE_reference,
                    n_values, n_values_reference
                ),
                .ksTest
            ),
            ksTestHarmonized = purrr::pmap(
                list(
                    decile_MEASUREMENT_VALUE_HARMONIZED, decile_MEASUREMENT_VALUE_HARMONIZED_reference,
                    n_values, n_values_reference
                ),
                .ksTest
            )
        )

    return(summary)
}


.ksTest <- function(deciles1, deciles2, n1, n2) {
    n1  <- as.double(n1)
    n2  <- as.double(n2)
    # Validate input parameters with checkmate
    if (is.null(deciles1) || is.null(deciles2)) {
        return(NULL)
    }
    checkmate::assert_tibble(deciles1, min.rows = 9, null.ok = FALSE)
    checkmate::assert_tibble(deciles2, min.rows = 9, null.ok = FALSE)
    checkmate::assert_subset(c("decile", "value"), names(deciles1))
    checkmate::assert_subset(c("decile", "value"), names(deciles2))
    if (!all(deciles1$decile == deciles2$decile)) stop("deciles1 and deciles2 must have the same deciles")

    if (n1 < 2 || n2 < 2) {
        return(NULL)
    }

    deciles <- deciles1$decile
    v1 <- deciles1$value
    v2 <- deciles2$value

    # CDF from deciles
    cdf <- function(x, p, v) {
        sapply(x, function(z) {
            if (z < min(v)) {
                0
            } else {
                max(p[v <= z])
            }
        })
    }

    # Evaluation grid
    x_all <- sort(unique(c(v1, v2)))

    # CDF values
    F1 <- cdf(x_all, deciles, v1)
    F2 <- cdf(x_all, deciles, v2)

    # KS statistic (lower bound)
    D_hat <- max(abs(F1 - F2))

    # Effective sample size
    n_eff <- (n1 * n2) / (n1 + n2)

    # KS p-value approximation
    lambda <- sqrt(n_eff) * D_hat

    p_value <- 2 * sum(
        sapply(1:100, function(k) {
            (-1)^(k - 1) * exp(-2 * k^2 * lambda^2)
        })
    )

    # Clamp to [0,1]
    p_value <- max(min(p_value, 1), 0)

    return(
        tibble(
            KS = D_hat,
            pValue = p_value
        )
    )
}










.summaryOfSummaryTable <- function(summary) {
    summary |> checkmate::assert_tibble()

    summary 
}

summary |> 
mutate(status = dplyr::case_when(
    OMOP_CONCEPT_ID == 0 ~ "unmapped",
    OMOP_CONCEPT_ID == -1 ~ "missing",
    TRUE ~ "mapped"
)) |> 
group_by(status) |>
summarise(
    n_concepts = dplyr::n(),
    n_records = sum(n_records),
    .groups = "drop"
) |> 
mutate(p_records = n_records / sum(n_records) * 100) |> 
arrange(dplyr::desc(status))





















