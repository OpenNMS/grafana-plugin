"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.AlarmHistogramCtrl = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _sdk = require("app/plugins/sdk");

var _lodash = require("lodash");

var _lodash2 = _interopRequireDefault(_lodash);

require("jquery.flot");

require("jquery.flot.selection");

require("jquery.flot.crosshair");

require("../../jquery.flot.categories");

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var AlarmHistogramCtrl = function (_MetricsPanelCtrl) {
    _inherits(AlarmHistogramCtrl, _MetricsPanelCtrl);

    function AlarmHistogramCtrl($scope, $injector) {
        _classCallCheck(this, AlarmHistogramCtrl);

        var _this = _possibleConstructorReturn(this, (AlarmHistogramCtrl.__proto__ || Object.getPrototypeOf(AlarmHistogramCtrl)).call(this, $scope, $injector));

        _this.scope = $scope;

        _lodash2.default.defaults(_this.panel, {
            groupProperty: 'acknowledged',
            direction: 'horizontal'
        });

        _this.events.on('init-edit-mode', _this.onInitEditMode.bind(_this));
        _this.events.on('data-received', _this.onDataReceived.bind(_this));
        _this.events.on('data-error', _this.onDataError.bind(_this));
        _this.events.on('data-snapshot-load', _this.onDataReceived.bind(_this));
        _this.events.on('render', _this.onRender.bind(_this));
        return _this;
    }

    _createClass(AlarmHistogramCtrl, [{
        key: "link",
        value: function link($scope, elem, attrs, ctrl) {
            this.elem = elem.find('.histogram-chart');
            this.ctrl = ctrl;
        }
    }, {
        key: "onInitEditMode",
        value: function onInitEditMode() {
            this.addEditorTab('Grouping', 'public/plugins/opennms-helm-app/panels/alarm-histogram/editor.html', 2);
        }
    }, {
        key: "onDataReceived",
        value: function onDataReceived(data) {
            switch (this.panel.groupProperty) {
                case 'acknowledged':
                    {
                        var counts = _lodash2.default.countBy(this.query(data, 'Acked By'), _lodash2.default.isNil);
                        this.series = [{
                            name: 'Outstanding',
                            count: _lodash2.default.defaultTo(counts[true], 0),
                            color: this.scope.$root.colors[0]
                        }, {
                            name: 'Acknowledged',
                            count: _lodash2.default.defaultTo(counts[false], 0),
                            color: this.scope.$root.colors[4]
                        }];
                        break;
                    }

                case 'severity':
                    {
                        var _counts = _lodash2.default.countBy(this.query(data, 'Severity'));
                        this.series = [{
                            name: 'Cleared',
                            count: _lodash2.default.defaultTo(_counts['CLEARED'], 0),
                            color: '#EEE000'
                        }, {
                            name: 'Normal',
                            count: _lodash2.default.defaultTo(_counts['NORMAL'], 0),
                            color: '#86B15B'
                        }, {
                            name: 'Indeterm.',
                            count: _lodash2.default.defaultTo(_counts['INDETERMINATE'], 0),
                            color: '#990000'
                        }, {
                            name: 'Warning',
                            count: _lodash2.default.defaultTo(_counts['WARNING'], 0),
                            color: '#FCCC3B'
                        }, {
                            name: 'Minor',
                            count: _lodash2.default.defaultTo(_counts['MINOR'], 0),
                            color: '#EE901C'
                        }, {
                            name: 'Major',
                            count: _lodash2.default.defaultTo(_counts['MAJOR'], 0),
                            color: '#E3692F'
                        }, {
                            name: 'Critical',
                            count: _lodash2.default.defaultTo(_counts['CRITICAL'], 0),
                            color: '#DB4345'
                        }];
                        break;
                    }
            }

            this.render();
        }
    }, {
        key: "onDataError",
        value: function onDataError() {
            this.series = [];
            this.render();
        }
    }, {
        key: "onRender",
        value: function onRender() {
            // Size handling
            if (this.elem.width() === 0) {
                return true;
            }

            var height = this.ctrl.height || this.ctrl.panel.height || this.ctrl.row.height;
            if (_lodash2.default.isString(height)) {
                height = parseInt(height.replace('px', ''), 10);
            }
            height -= 5; // padding
            height -= this.ctrl.panel.title ? 24 : 9; // subtract panel title bar

            this.elem.css('height', height + 'px');

            // Draw graph
            switch (this.panel.direction) {
                case 'horizontal':
                    $.plot(this.elem, _lodash2.default.map(this.series, function (serie) {
                        return {
                            data: [[serie.count, serie.name]],
                            color: serie.color
                        };
                    }), {
                        series: {
                            bars: {
                                show: true,
                                barWidth: 0.6,
                                align: "center",
                                fill: 0.8,
                                lineWidth: 1.0,
                                horizontal: true
                            }
                        },
                        yaxis: {
                            mode: "categories",
                            tickLength: 0,
                            autoscaleMargin: .02
                        },
                        grid: {
                            borderWidth: 0
                        }
                    });
                    break;

                case 'vertical':
                    $.plot(this.elem, _lodash2.default.map(this.series, function (serie) {
                        return {
                            data: [[serie.name, serie.count]],
                            color: serie.color
                        };
                    }), {
                        series: {
                            bars: {
                                show: true,
                                barWidth: 0.5,
                                align: "center",
                                fill: 0.8,
                                lineWidth: 1.0,
                                horizontal: false
                            }
                        },
                        xaxis: {
                            mode: "categories",
                            tickLength: 0,
                            autoscaleMargin: .02
                        },
                        grid: {
                            borderWidth: 0
                        }
                    });
                    break;
            }
        }
    }, {
        key: "query",
        value: function query(data, column) {
            // TODO: Make this a generator to save memory
            var result = [];

            for (var i = 0; i < data.length; i++) {
                var columnIndex = _lodash2.default.findIndex(data[i].columns, { text: column });
                for (var j = 0; j < data[i].rows.length; j++) {
                    result.push(data[i].rows[j][columnIndex]);
                }
            }

            return result;
        }
    }]);

    return AlarmHistogramCtrl;
}(_sdk.MetricsPanelCtrl);

AlarmHistogramCtrl.templateUrl = 'panels/alarm-histogram/module.html';

exports.AlarmHistogramCtrl = AlarmHistogramCtrl;
//# sourceMappingURL=ctrl.js.map
