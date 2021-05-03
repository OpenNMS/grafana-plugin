/*
The MIT License (MIT)

Copyright (c) 2016 Grafana

                  Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
                                                                     in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                                                                 copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                                 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                                 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                                 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                                 SOFTWARE.
*/

// This file was adapted from the Grafana piechart plugin located here: https://github.com/grafana/piechart-panel

import angular from "angular";
import $ from "jquery";
import _ from "lodash";
import PerfectScrollbar from "perfect-scrollbar";

angular
    .module("grafana.directives")
    .directive("flowLegend", function (popoverSrv, $timeout) {
        return {
            link: function (scope, elem) {
                const $container = $('<div class="flow-legend__container"></div>');
                let firstRender = true;
                const ctrl = (scope as any).ctrl;
                const panel = ctrl.panel;
                let data;
                let i;
                let legendScrollbar;

                scope.$on("$destroy", function () {
                    if (legendScrollbar) {
                        legendScrollbar.destroy();
                    }
                });

                ctrl.events.on("render", function () {
                    data = ctrl.legendData;

                    if (data) {
                        render();
                    }
                });

                function getSeriesIndexForElement(el) {
                    return el.parents("[data-series-index]").data("series-index");
                }

                function toggleSeries(e) {
                    const el = $(e.currentTarget);
                    const index = getSeriesIndexForElement(el);
                    const scrollPosition = $($container.children("tbody")).scrollTop() || 0;
                    ctrl.toggleSeries(index);
                    $($container.children("tbody")).scrollTop(scrollPosition);
                }

                function openColorSelector(e) {
                    // if we clicked inside popup container ignore click
                    if ($(e.target).parents(".popover").length) {
                        return;
                    }

                    const el = $(e.currentTarget).find(".fa-minus");
                    const index = getSeriesIndexForElement(el);

                    $timeout(function () {
                        popoverSrv.show({
                            element: el[0],
                            position: 'right center',
                            template:
                                '<series-color-picker-popover series="series" onToggleAxis="toggleAxis" onColorChange="colorSelected">' +
                                "</series-color-picker-popover>",
                            openOn: "hover",
                            classNames: 'drop-popover drop-popover--transparent',
                            model: {
                                autoClose: true,
                                colorSelected: function (color) {
                                    ctrl.changeSeriesColor(index, color);
                                }
                            }
                        });
                    });
                }

                function render() {
                    if (!panel.legend.show) {
                        $container.empty();
                        elem.find(".flow-legend").css("padding-top", 0);
                        return;
                    } else {
                        elem.find(".flow-legend").css("padding-top", 6);
                    }

                    if (firstRender) {
                        elem.append($container);
                        $container.on("click", ".flow-legend-icon", openColorSelector);
                        $container.on("click", ".flow-legend-alias", toggleSeries);
                        firstRender = false;
                    }

                    $container.empty();

                    const width = panel.legendType === "Right side" && panel.legend.sideWidth ? panel.legend.sideWidth + "px" : "";
                    const ieWidth = panel.legendType === "Right side" && panel.legend.sideWidth ? panel.legend.sideWidth - 1 + "px" : "";
                    elem.css("min-width", width);
                    elem.css("width", ieWidth);

                    $container.toggleClass("flow-legend-table", false);

                    let seriesElements = [] as Array<JQuery<HTMLElement>>;

                    for (i = 0; i < data.length; i++) {
                        const seriesData = data[i];

                        let html = '<div class="flow-legend-series';
                        if (ctrl.hiddenSeries[i]) {
                            html += " flow-legend-series-hidden";
                        }
                        html += '" data-series-index="' + i + '">';
                        html += '<span class="flow-legend-icon" style="float:none;">';
                        html +=
                            '<i class="fa fa-minus pointer" style="color:' +
                            seriesData.color +
                            '"></i>';
                        html += "</span>";

                        html +=
                            '<a class="flow-legend-alias" style="float:none;">' +
                            _.escape(seriesData.label) +
                            "</a>";

                        html += "</div>";

                        seriesElements.push($(html));
                    }

                    $container.append(seriesElements);

                    if (panel.legendType === "Under graph" || panel.legendType === "Right side") {
                        addScrollbar($container);
                    } else {
                        destroyScrollbar();
                    }
                }

                function addScrollbar(_container: any) {
                    const scrollbarOptions = {
                        // Number of pixels the content height can surpass the container height without enabling the scroll bar.
                        scrollYMarginOffset: 2,
                        suppressScrollX: true
                    };

                    if (!legendScrollbar) {
                        legendScrollbar = new PerfectScrollbar(
                            elem[0],
                            scrollbarOptions
                        );
                    } else {
                        legendScrollbar.update();
                    }
                }

                function destroyScrollbar() {
                    if (legendScrollbar) {
                        legendScrollbar.destroy();
                        legendScrollbar = null;
                    }
                }
            }
        };
    });
