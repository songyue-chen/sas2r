---
name: sas-native-graphics
description: Implement source-defined graphics and ODS output with native R drawing functions and a working template consumer.
metadata:
  sas2r:
    version: 1
    agents: [translator, reviewer, fixer]
    priority: 90
    triggers:
      flags: [native_graphics]
    tools: [read_dependency_context, lookup_rulebook]
---

# Native R graphics from SAS source

PROC TEMPLATE / DEFINE STATGRAPH describes a drawing, PROC SGRENDER supplies
data and dynamic parameters, and ODS selects the destination. Translate those
used effects together. There is no requirement to implement a generic GTL
interpreter, SAS template catalog or a function named SGRENDER in R.

Read the source template and the selected R dependency before deciding how its
caller renders the figure. If the initial packet is truncated, retrieve the
needed dependency pages with read_dependency_context. For a reusable template,
a returned drawing function or explicit specification with a working consumer
can be appropriate. The caller must capture and use it; calling a function that
merely returns unused metadata does not implement template registration.

Use permitted packages: prefer ggplot2 when it is allowlisted and installed,
drawing the source-supplied statistics with ggplot2::geom_boxplot(stat = "identity")
or the matching geom rather than recomputing them; graphics, grDevices and grid
remain available and are the fallback when a ggplot2 layer cannot express the
required drawing.
Do not assume a plotting function's default calculations match SAS. Apply the
statistical-defaults guidance to quartiles, whiskers, notches, weights and
outliers. Supply computed statistics directly when required. The absence of a
SAS-specific helper is not a reason to insert a stopping stub.

Trace the source through the last consumer. Preserve grouping and ordering,
discrete versus continuous axes, reference lines, mean and outlier markers,
legends, labels, requested summary-table rows, titles/footnotes, page limits and
file naming. Keep table labels and values inside the device bounds; disabling
panel clipping does not make text outside the PDF page visible. Reserve a
label column or adequate margins. Create the output directory if needed.
Open a multipage PDF once around the page loop, and close it even
if drawing fails. Do not overwrite it once per page. Fonts and spacing can
differ; omitted statistics and table rows are not cosmetic differences.

This small native template illustrates the producer/consumer interface only.
Its caller supplies already established statistics, table text and labels;
adapt the contents to the actual source, rather than claiming this implements
every GTL template. `notch` and the associated `box$conf` must follow the source.

```r
summary_template <- function(width = 7, height = 6) {
  function(pages, output_file) {
    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
    grDevices::pdf(output_file, width = width, height = height, onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
    for (page in pages) {
      graphics::layout(matrix(c(1, 2), ncol = 1), heights = c(3, 2))
      graphics::par(mar = c(3, 4, 3, 1))
      graphics::bxp(page$box, notch = page$notch, ylim = page$ylim,
                    ylab = page$ylab, main = page$title)
      if (length(page$references)) graphics::abline(h = page$references, col = "red")
      if (length(page$means)) graphics::points(seq_along(page$means), page$means, pch = 18)
      graphics::par(mar = c(1, 1, 1, 1))
      tab <- page$table
      graphics::plot.new()
      graphics::plot.window(xlim = c(0, ncol(tab) + 1), ylim = c(0, nrow(tab) + 1))
      for (j in seq_len(ncol(tab))) {
        graphics::text(j, rev(seq_len(nrow(tab))), labels = tab[, j])
      }
      graphics::mtext(page$footnote, side = 1, line = 0)
    }
    invisible(output_file)
  }
}
# The calling program retains the translated template and invokes it:
draw_summary <- summary_template()
# draw_summary(pages, output_file)  # pages are constructed from the source rules
```

A PDF header or nonempty file proves neither statistical equivalence nor
complete figure content. Review the values actually supplied to the drawing
function, then inspect the rendered pages and required annotations. A missing
source definition remains uncertainty; a defined drawing replaced by a stop
remains an implementation defect until fixed.

References:
- [R bxp: draw box plots from supplied summaries](https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/bxp.html)
- [R PDF graphics device](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/pdf.html)
- [SAS SGRENDER dynamic variables](https://support.sas.com/documentation/cdl/en/grstatproc/62603/HTML/default/a003239624.htm)
