



textContent.time.series <- "<strong>Overview of Expert Demographics:</strong>      <br/>
<br/>
<table class='table table-bordered'>
            <thead>
              <tr>
                <th></th>
                <th>Europe</th>
                <th>USA</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>October 2022</td>
                <td>Academics: 12<br>Practitioners<strong>*</strong>: 10</td>
                <td>Academics: 23<br>Practitioners<strong>*</strong>: 19</td>
              </tr>
              <tr>
                <td>August 2023</td>
                <td>Academics: 27<br>Practitioners<strong>*</strong>: 14<br>Average Years of Experience in Digital Privacy: > 10 years</td>
                <td>Academics: 22<br>Practitioners<strong>*</strong>: 11<br>Average Years of Experience in Digital Privacy: > 10 years</td>
              </tr>
            </tbody>
          </table>
<strong>*Practitioners include experts from:</strong> Tech industry, Non-Tech industry, Government, Journalism, Law, Non-profit / NGO / Think tank  <br/>
              "


textContent.wave1 <- "<strong>Overview of Expert Demographics:</strong>      <br/>
<br/>
<table class='table table-bordered'>
            <thead>
              <tr>
                <th></th>
                <th>Europe</th>
                <th>USA</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>October 2022</td>
                <td>Academics: 12<br>Practitioners<strong>*</strong>: 10</td>
                <td>Academics: 23<br>Practitioners<strong>*</strong>: 19</td>
              </tr>
            </tbody>
          </table>
<strong>*Practitioners include experts from:</strong> Tech industry, Non-Tech industry, Government, Journalism, Law, Non-profit / NGO / Think tank  <br/>
              "



textContent.wave2 <- "<strong>Overview of Expert Demographics:</strong>      <br/>
<br/>
<table class='table table-bordered'>
            <thead>
              <tr>
                <th></th>
                <th>Europe</th>
                <th>USA</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>August 2023</td>
                <td>Academics: 27<br>Practitioners<strong>*</strong>: 14<br>Average Years of Experience in Digital Privacy:  > 10 years</td>
                <td>Academics: 22<br>Practitioners<strong>*</strong>: 11<br>Average Years of Experience in Digital Privacy:  > 10 years</td>
              </tr>
            </tbody>
          </table>
<strong>*Practitioners include experts from:</strong> Tech industry, Non-Tech industry, Government, Journalism, Law, Non-profit / NGO / Think tank  <br/>
              "


textContent.wave3 <- "<strong>Overview of Expert Demographics:</strong>      <br/>
<br/>
<table class='table table-bordered'>
            <thead>
              <tr>
                <th></th>
                <th>Europe</th>
                <th>USA</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>December 2023</td>
                <td>Academics: 23<br>Practitioners<strong>*</strong>: 21<br>Average Years of Experience in Digital Privacy:  > 10 years</td>
                <td>Academics: 24<br>Practitioners<strong>*</strong>: 11<br>Average Years of Experience in Digital Privacy:  > 10 years</td>
              </tr>
            </tbody>
          </table>
<strong>*Practitioners include experts from:</strong> Tech industry, Non-Tech industry, Government, Journalism, Law, Non-profit / NGO / Think tank  <br/>
              "

resize_js <- "
  function(el) {
    var chartDom = el;
    var myChart = echarts.getInstanceByDom(chartDom);

    function resizeChart() {
      var option = myChart.getOption();
      var newFontSize = window.innerWidth < 768 ? 9 : 14; // Use numbers for font sizes
      if(option.xAxis && option.xAxis[0] && option.xAxis[0].axisLabel && option.xAxis[0].axisLabel.textStyle) {
        option.xAxis[0].axisLabel.textStyle.fontSize = newFontSize;
      }
      myChart.setOption(option, true); // Use `true` to not merge with previous option
    }

    // Resize the chart when the window is resized
    window.addEventListener('resize', resizeChart);

    // Call resizeChart to set the initial font size
    resizeChart();
  }
"


createRedLine <- function(){
  return('<hr style="width: 50px; text-align: left; margin-left: 0; margin-top: 7px; margin-bottom: 25px; border-top: 3px solid #e73f0c;">')
}

sharingInSocialMedia <- function(sharer_link, website, network) {
  tags$li(
    class = "dropdown",
    tags$a(
      href = str_c(sharer_link, website),
      target = "_blank",
      tags$img(
        height = "20px",
        src = str_c(network, ".png")
      )
    )
  )
}

# (4-1) mobileDetect: Die Funktion dient dazu, die Grafiken anzupassen, wenn wir den Datenportal mit Tablet oder Handy angucken.
# Das Problem ist, sie erkennt bestimmte Maßen wie iphone 6/7/8. 
# Also die Grafiken werden nicht angepasst, zum Beispiel wenn wir den Datenportal im PC mit kleinen Fenster angucken. 

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


#' Setup toolbox
#'

#'

#' @export
etoolbox <- function(p, show = TRUE, zlevel = 0, z = 6, orient = "horizontal", x = "right", y = "top",
                     backgroundColor = "rgba(0,0,0,0)", borderColor = "#ccc", borderWidth = 0, padding = 5,
                     itemGap = 10, itemSize = 16, color = NULL, disableColor = "#ddd", effectiveColor = "red",
                     showTitle = TRUE, textStyle = NULL, ...){
  
  if(is.null(textStyle)) textStyle <- list(fontFamily = "Arial, Verdana, sans-serif", fontSize = 12,
                                           fontStyle = "normal", fontWeight = "normal")
  
  color <- if(is.null(color)) list("#1e90ff", "#22bb22", "#4b0082", "#d2691e") else color
  
  opts <- list(...)
  opts$show <- show
  opts$zlevel <- zlevel
  opts$z <- z
  opts$orient <- orient
  opts$x <- x
  opts$y <- y
  opts$backgroundColor <- backgroundColor
  opts$borderColor <- borderColor
  opts$borderWidth <- borderWidth
  opts$padding <- padding
  opts$itemGap <- itemGap
  opts$itemSize <- itemSize
  opts$color <- color
  opts$disableColor <- disableColor
  opts$effectiveColor <- effectiveColor
  opts$showTitle <- showTitle
  opts$textStyle <- textStyle
  
  p$x$options$toolbox <- opts
  
  p
  
}

add_toolbox_elem <- function(p, opts, elem){
  
  tb <- p$x$options$toolbox
  
  if(!length(tb)){
    p <- p %>%
      etoolbox()
  }
  
  p$x$options$toolbox$feature[[elem]] <- opts
  
  p
  
}

#' Add toolbox feature
#'
#' Add toolbox feature.
#'
#' @param p an echart object.
#' @param mark markLine icons see \code{\link{etoolbox_mark}}.
#' @param dataZoom dataZoom icons \code{\link{etoolbox_zoom}}.
#' @param dataView dataView icons \code{\link{etoolbox_view}}.
#' @param magicType magicType icons \code{\link{etoolbox_magic}}.
#' @param restore restore icon \code{\link{etoolbox_restore}}.
#' @param saveAsImage saveAsImage icon \code{\link{etoolbox_save}}.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_magic(type = list("line", "bar")) %>%
#'   etoolbox_feature(restore = list(show = TRUE))
#'
#' @export
etoolbox_feature <- function(p, mark, dataZoom, dataView, magicType, restore, saveAsImage){
  
  opts <- list()
  opts$mark <- if(!missing(mark)) mark
  opts$dataZoom <- if(!missing(dataZoom)) dataZoom
  opts$dataView <- if(!missing(magicType)) magicType
  opts$restore <- if(!missing(restore)) restore
  opts$saveAsImage <- if(!missing(saveAsImage)) saveAsImage
  
  p$x$options$toolbox$feature <- append(p$x$options$toolbox$feature, opts)
  
  p
}

#' Add toolbox feature mark button
#'
#' Enable marking chart.
#'
#' @param p an echart object.
#' @param show whether to show mark.
#' @param title mark button title.
#' @param lineStyle style of marked line.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_mark()
#'
#' @export
etoolbox_mark <- function(p, show = TRUE, title = list(mark = "Mark", markUndo = "Undo", markClear = "Clear"),
                          lineStyle = list(color = "#1e90ff", typed = "dashed", width = 2, shadowColor = "rgba(0,0,0,0)",
                                           shadowBlur = 5, shadowOffsetX = 3, shadowOffsetY = 3)){
  
  opts <- list()
  opts$show <- show
  opts$title <- title
  opts$lineStyle <- lineStyle
  
  p <- add_toolbox_elem(p, opts, "mark")
  
  p
  
}

#' Add toolbox zoom button
#'
#' Add zoom feature.
#'
#' @param p an echart object.
#' @param show whether to show zoom.
#' @param title button title.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_zoom()
#'
#' @export
etoolbox_zoom <- function(p, show = TRUE, title = list(dataZoom = "Area Zoom", dataZoomReset = "Reset")){
  
  opts <- list()
  opts$show <- show
  opts$title <- title
  
  p <- add_toolbox_elem(p, opts, "dataZoom")
  
  p
  
}

#' Add toolbox data view
#'
#' Enables viewing data table.
#'
#' @param p an echart object.
#' @param show whether to show data view.
#' @param title button title.
#' @param readOnly set as read-only.
#' @param lang default text.
#' @param ... any other parameters to pass to data view.
#'
#' @examples
#' mtcars %>%
#'   echart(qsec) %>%
#'   ebar(mpg) %>%
#'   etoolbox() %>%
#'   etoolbox_view()
#'
#' @export
etoolbox_view <- function(p, show = TRUE, title = "View", readOnly = FALSE, lang = list('Data View', 'close', 'refresh'), ...){
  
  opts <- list(...)
  opts$show <- show
  opts$title <- title
  opts$readOnly <- readOnly
  opts$lang <- lang
  
  p <- add_toolbox_elem(p, opts, "dataView")
  
  p
  
}

#' Add toolbox magic buttons
#'
#' Enable changing chart type.
#'
#' @param p an echart object.
#' @param show wehtehr to show magic buttons.
#' @param type chart types, see details.
#' @param title titles of charts.
#' @param ... any other options to pass to magic feature.
#'
#' @details
#' Pass a \code{list} to \code{type}, valid values are:
#'
#' \itemize{
#'   \item{\code{line}}
#'   \item{\code{bar}}
#'   \item{\code{stack}}
#'   \item{\code{tiled}}
#'   \item{\code{force}}
#'   \item{\code{chord}}
#'   \item{\code{pie}}
#'   \item{\code{funnel}}
#' }
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   ebar(wt) %>% # not stacked
#'   etooltip() %>%
#'   elegend() %>%
#'   etoolbox() %>%
#'   etoolbox_magic(type = list("bar", "line", "stack", "tiled"))
#'
#' @export
etoolbox_magic <- function(p, show = TRUE, type = list(), title, ...){
  
  title <- if(missing(title)) list(line = "line",
                                   bar = "bar",
                                   stack = "stack",
                                   tiled = "tiled",
                                   force = "force",
                                   chord = "chord",
                                   pie = "pie",
                                   funnel = "funnel")
  
  opts <- list(...)
  opts$show <- show
  opts$title <- title
  opts$type <- type
  
  p <- add_toolbox_elem(p, opts, "magicType")
  
  p
  
}

#' Add toolbox restore button
#'
#' Add toolbox restore button.
#'
#' @param p an echart object.
#' @param show whether to show button.
#' @param title title of button.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   ebar(wt) %>% # not stacked
#'   etoolbox_restore()
#'
#' @export
etoolbox_restore <- function(p, show = TRUE, title = "Reset"){
  
  opts <- list()
  opts$show <- show
  opts$title <- title
  
  p <- add_toolbox_elem(p, opts, "restore")
  
  p
  
}

#' Add toolbox save as image button
#'
#' Add save as image button.
#'
#' @param p an echart object.
#' @param show whether to show the button.
#' @param title title of button.
#' @param type image type
#' @param name of file.
#' @param lang text.
#'
#' @examples
#' mtcars %>%
#'   echart(disp) %>%
#'   ebar(mpg, stack = "grp") %>% # stack
#'   ebar(qsec, stack = "grp") %>% # stack
#'   etoolbox() %>%
#'   etoolbox_save()
#'
#'
#' @export
etoolbox_save <- function(p, show = TRUE, title = "Save as image", type = "png", name = "echarts", lang = "Save"){
  
  opts <- list()
  opts$show <- show
  opts$title <- title
  
  p <- add_toolbox_elem(p, opts, "saveAsImage")
  
  p
  
}

#' Add all elements of toolbox
#'
#' Adds toolbok mark, restor, save, and view.
#'
#' @param p an echart object.
#' @param ... any other option to pass to \code{\link{etoolbox}}.
#'
#' @details Adds mark, restore, save, view and zoom buttons
#'
#' @export
etoolbox_full <- function(p, ...){
  
  p <- p %>%
    etoolbox(...) %>%
    etoolbox_mark() %>%
    etoolbox_restore() %>%
    etoolbox_save() %>%
    etoolbox_zoom() %>%
    etoolbox_view()
  
  p
}
