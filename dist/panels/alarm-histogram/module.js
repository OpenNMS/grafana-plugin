define(["lodash","jquery","@grafana/runtime","app/plugins/sdk","jquery.flot","jquery.flot.selection","jquery.flot.crosshair"],(function(t,e,r,n,o,i,a){return function(t){var e={};function r(n){if(e[n])return e[n].exports;var o=e[n]={i:n,l:!1,exports:{}};return t[n].call(o.exports,o,o.exports,r),o.l=!0,o.exports}return r.m=t,r.c=e,r.d=function(t,e,n){r.o(t,e)||Object.defineProperty(t,e,{enumerable:!0,get:n})},r.r=function(t){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},r.t=function(t,e){if(1&e&&(t=r(t)),8&e)return t;if(4&e&&"object"==typeof t&&t&&t.__esModule)return t;var n=Object.create(null);if(r.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:t}),2&e&&"string"!=typeof t)for(var o in t)r.d(n,o,function(e){return t[e]}.bind(null,o));return n},r.n=function(t){var e=t&&t.__esModule?function(){return t.default}:function(){return t};return r.d(e,"a",e),e},r.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},r.p="/",r(r.s=51)}({0:function(e,r){e.exports=t},2:function(t,e,r){"use strict";r.d(e,"a",(function(){return n}));function n(t,e,r,n){return new(r||(r=Promise))((function(o,i){function a(t){try{s(n.next(t))}catch(t){i(t)}}function u(t){try{s(n.throw(t))}catch(t){i(t)}}function s(t){var e;t.done?o(t.value):(e=t.value,e instanceof r?e:new r((function(t){t(e)}))).then(a,u)}s((n=n.apply(t,e||[])).next())}))}Object.create;Object.create},25:function(t,e){t.exports=o},26:function(t,e){t.exports=i},27:function(t,e){t.exports=a},28:function(t,e){!function(t){function e(t,e,r,n){var o="categories"==e.xaxis.options.mode,i="categories"==e.yaxis.options.mode;if(o||i){var a=n.format;if(!a){var u=e;if((a=[]).push({x:!0,number:!0,required:!0}),a.push({y:!0,number:!0,required:!0}),u.bars.show||u.lines.show&&u.lines.fill){var s=!!(u.bars.show&&u.bars.zero||u.lines.show&&u.lines.zero);a.push({y:!0,number:!0,required:!1,defaultValue:0,autoscale:s}),u.bars.horizontal&&(delete a[a.length-1].y,a[a.length-1].x=!0)}n.format=a}for(var c=0;c<a.length;++c)a[c].x&&o&&(a[c].number=!1),a[c].y&&i&&(a[c].number=!1)}}function r(t){var e=[];for(var r in t.categories){var n=t.categories[r];n>=t.min&&n<=t.max&&e.push([n,r])}return e.sort((function(t,e){return t[0]-e[0]})),e}function n(e,n,o){if("categories"==e[n].options.mode){if(!e[n].categories){var i={},a=e[n].options.categories||{};if(t.isArray(a))for(var u=0;u<a.length;++u)i[a[u]]=u;else for(var s in a)i[s]=a[s];e[n].categories=i}e[n].options.ticks||(e[n].options.ticks=r),function(t,e,r){for(var n=t.points,o=t.pointsize,i=t.format,a=e.charAt(0),u=function(t){var e=-1;for(var r in t)t[r]>e&&(e=t[r]);return e+1}(r),s=0;s<n.length;s+=o)if(null!=n[s])for(var c=0;c<o;++c){var l=n[s+c];null!=l&&i[c][a]&&(l in r||(r[l]=u,++u),n[s+c]=r[l])}}(o,n,e[n].categories)}}function o(t,e,r){n(e,"xaxis",r),n(e,"yaxis",r)}t.plot.plugins.push({init:function(t){t.hooks.processRawData.push(e),t.hooks.processDatapoints.push(o)},options:{xaxis:{categories:null},yaxis:{categories:null}},name:"categories",version:"1.0"})}(jQuery)},3:function(t,e,r){"use strict";r.d(e,"e",(function(){return p})),r.d(e,"c",(function(){return h})),r.d(e,"h",(function(){return m})),r.d(e,"i",(function(){return y})),r.d(e,"k",(function(){return b})),r.d(e,"d",(function(){return w})),r.d(e,"f",(function(){return O})),r.d(e,"g",(function(){return j})),r.d(e,"a",(function(){return k})),r.d(e,"j",(function(){return A})),r.d(e,"b",(function(){return E}));var n=r(2),o=r(0),i=r.n(o),a=r(8);function u(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}function s(t,e){for(var r=0;r<e.length;r++){var n=e[r];n.enumerable=n.enumerable||!1,n.configurable=!0,"value"in n&&(n.writable=!0),Object.defineProperty(t,n.key,n)}}function c(t,e,r){return e&&s(t.prototype,e),r&&s(t,r),Object.defineProperty(t,"prototype",{writable:!1}),t}function l(t){return function(t){if(Array.isArray(t))return f(t)}(t)||function(t){if("undefined"!=typeof Symbol&&null!=t[Symbol.iterator]||null!=t["@@iterator"])return Array.from(t)}(t)||function(t,e){if(!t)return;if("string"==typeof t)return f(t,e);var r=Object.prototype.toString.call(t).slice(8,-1);"Object"===r&&t.constructor&&(r=t.constructor.name);if("Map"===r||"Set"===r)return Array.from(t);if("Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r))return f(t,e)}(t)||function(){throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function f(t,e){(null==e||e>t.length)&&(e=t.length);for(var r=0,n=new Array(e);r<e;r++)n[r]=t[r];return n}var d=!1;function p(){d||(d=!0,Object(a.loadPluginCss)({dark:"plugins/opennms-helm-app/styles/dark.css",light:"plugins/opennms-helm-app/styles/light.css"}))}function h(t,e){var r=arguments.length>2&&void 0!==arguments[2]?arguments[2]:{};for(var n in r)r.hasOwnProperty(n)&&(t[n]=void 0===e[n]?r[n]:e[n])}function v(t){var e,r,n,o,a=[];for(e=0;e<t.options.length;e++)if((o=t.options[e]).selected=!1,i.a.isArray(t.current.value))for(r=0;r<t.current.value.length;r++)n=t.current.value[r],o.value===n&&(o.selected=!0,a.push(o));else o.value===t.current.value&&(o.selected=!0,a.push(o));return a}function m(t,e){return t.current=i.a.cloneDeep(e||{}),i.a.isArray(t.current.text)&&t.current.text.length>0?t.current.text=t.current.text.join(" + "):i.a.isArray(t.current.value)&&"$__all"!==t.current.value[0]&&(t.current.text=t.current.value.join(" + ")),v(t),function(t){var e=arguments.length>1&&void 0!==arguments[1]&&arguments[1],r=t.dashboardSrv,n=t.templateSrv;if(t.initLock)return Promise.resolve();var o=n.getVariables.bind(n)||r.dashboard.getVariables.bind(r.dashboard),i=o(),a=[];return i.forEach((function(t){return a.push(t.updateOptions())})),n.setGlobalVariable(t.id,t.current),Promise.all(a).then((function(){e&&(r.dashboard.templateVariableValueUpdated(),r.dashboard.startRefresh())}))}(t)}function y(t,e){var r=Promise.resolve();return t.refresh&&(r=t.updateOptions()),r.then((function(){var r=i.a.find(t.options,(function(t){return t.text===e||t.value===e}));if(!r){var n=e,o=e;i.a.isArray(e)&&(n=e.reduce((function(e,r){var n=i.a.find(t.options,{value:r});return n?e.push(n.text):e.push(r),e}),[])),r={text:n,value:o}}return t.multi&&(r={text:i.a.castArray(r.text),value:i.a.castArray(r.value)}),t.setValue(r)}))}function b(t,e){if(t.current||(t.current={}),i.a.isArray(t.current.value)){var r=v(t);if(0===r.length){var n=t.options[0];return n?t.setValue(n):Promise.resolve()}var o={value:i.a.map(r,(function(t){return t.value})),text:i.a.map(r,(function(t){return t.text}))};return t.setValue(o)}var a=void 0;return(a=i.a.find(t.options,{text:t.current.text}))||e&&(a=i.a.find(t.options,{text:e}))?t.setValue(a):t.options?t.setValue(t.options[0]):Promise.resolve()}var g=/\$(\w+)|\[\[([\s\S]+?)(?::(\w+))?\]\]|\${(\w+)(?:\.([^:^}]+))?(?::(\w+))?}/g,x=function(t){return g.lastIndex=0,g.exec(t)};function w(){for(var t=arguments.length,e=new Array(t),r=0;r<t;r++)e[r]=arguments[r];var n=e[e.length-1];e[0]=i.a.isString(e[0])?e[0]:Object.values(e[0]).join(" ");var o=e.slice(0,-1).join(" "),a=o.match(g),u=null!==a&&a.find((function(t){var e=x(t);return null!==e&&e.indexOf(n)>-1}));return!!u}function O(t,e,r){return r.indexOf(t)===e}function j(t){if(t){var e,r=t.map((function(t){return function(t,e,r){if(t){if(t.startsWith("{")&&t.endsWith("}")){var n=t.substring(1,t.length-1).split(",").map((function(t){return t.trim()}));return r&&n.some((function(t){return"all"===t}))?[]:n}return e&&t.startsWith("$")||r&&"all"===t?[]:[t]}return[]}(t,!0,!1)}));return r.some((function(t){return t.some((function(t){return"all"===t}))}))?[]:(e=[]).concat.apply(e,l(r)).filter(O)}return[]}var k=function(){function t(){u(this,t)}return c(t,null,[{key:"getGlobAsRegexPattern",value:function(t){return i.a.escapeRegExp(t).replace(/\\\*/gi,".*").replace(/\\\|/gi,"|")}},{key:"hasGlob",value:function(e){return i.a.some(l(e),(function(e){return i.a.includes(t.globExpressions,e)}))}}]),t}();function R(t,e,r){var n=t[e];return t[e]=t[r],t[r]=n,t}function A(t,e,r){if(t&&t.length>0&&e>=0&&r>=0)for(var n=0;n<t.length;n++){if(e>=t[n].length||r>=t[n].length)throw new Error("Index out of bounds");t[n]=R(t[n],e,r)}return t}k.globExpressions=["*","|"];var E=function(){function t(e,r){u(this,t),this.timeout=1e4,this.withCredentials=!1,this.searchLimit=25,this.backendSrv=e,this.url=r}return c(t,[{key:"doOpenNMSRequest",value:function(t){return(this.basicAuth||this.withCredentials)&&(t.withCredentials=!0),this.basicAuth&&(t.headers=t.headers||{},t.headers.Authorization=this.basicAuth),t.url=this.url+t.url,this.timeout&&(t.timeout=this.timeout),this.backendSrv.datasourceRequest(t)}},{key:"getLocations",value:function(){var t=arguments.length>0&&void 0!==arguments[0]?arguments[0]:0;return this.doOpenNMSRequest({url:"/rest/monitoringLocations",method:"GET",params:{limit:t}}).then((function(t){t.data.count,t.data.totalCount;var e=[];return i.a.each(t.data.location,(function(t){var r=t["location-name"]?t["location-name"].toString():null,n=i.a.find(e,(function(t){return t.text===r}));r&&!n&&e.push({text:r,value:r,expandable:!0})})),e}))}},{key:"getNodesByFilter",value:function(t){return Object(n.a)(this,void 0,void 0,regeneratorRuntime.mark((function e(){var r;return regeneratorRuntime.wrap((function(e){for(;;)switch(e.prev=e.next){case 0:return e.next=2,this.doOpenNMSRequest({url:"/rest/nodes",method:"GET",params:{filterRule:t,limit:0}});case 2:return(r=e.sent).data.count,r.data.totalCount,e.abrupt("return",r.data.node);case 5:case"end":return e.stop()}}),e,this)})))}}]),t}()},51:function(t,e,r){"use strict";r.r(e);var n=r(9),o=r(0),i=r.n(o),a=r(6),u=r.n(a);r(25),r(26),r(27),r(28);function s(t){return(s="function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?function(t){return typeof t}:function(t){return t&&"function"==typeof Symbol&&t.constructor===Symbol&&t!==Symbol.prototype?"symbol":typeof t})(t)}function c(t,e){for(var r=0;r<e.length;r++){var n=e[r];n.enumerable=n.enumerable||!1,n.configurable=!0,"value"in n&&(n.writable=!0),Object.defineProperty(t,n.key,n)}}function l(t,e){return(l=Object.setPrototypeOf||function(t,e){return t.__proto__=e,t})(t,e)}function f(t){var e=function(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Boolean.prototype.valueOf.call(Reflect.construct(Boolean,[],(function(){}))),!0}catch(t){return!1}}();return function(){var r,n=h(t);if(e){var o=h(this).constructor;r=Reflect.construct(n,arguments,o)}else r=n.apply(this,arguments);return d(this,r)}}function d(t,e){if(e&&("object"===s(e)||"function"==typeof e))return e;if(void 0!==e)throw new TypeError("Derived constructors may only return object or undefined");return p(t)}function p(t){if(void 0===t)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return t}function h(t){return(h=Object.setPrototypeOf?Object.getPrototypeOf:function(t){return t.__proto__||Object.getPrototypeOf(t)})(t)}var v=function(t){a.$inject=["$scope","$injector","$timeout"],function(t,e){if("function"!=typeof e&&null!==e)throw new TypeError("Super expression must either be null or a function");t.prototype=Object.create(e&&e.prototype,{constructor:{value:t,writable:!0,configurable:!0}}),Object.defineProperty(t,"prototype",{writable:!1}),e&&l(t,e)}(a,t);var e,r,n,o=f(a);function a(t,e,r){var n;return function(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}(this,a),(n=o.call(this,t,e)).scope=t,n.$timeout=r,n._renderRetries=0,i.a.defaults(n.panel,{groupProperty:"acknowledged",direction:"horizontal"}),n.retryTimes=10,n.retryDelay=100,n.events.on("init-edit-mode",n.onInitEditMode.bind(p(n))),n.events.on("data-received",n.onDataReceived.bind(p(n))),n.events.on("data-error",n.onDataError.bind(p(n))),n.events.on("data-snapshot-load",n.onDataReceived.bind(p(n))),n.events.on("render",n.onRender.bind(p(n))),n}return e=a,(r=[{key:"link",value:function(t,e,r,n){this.elem=e.find(".histogram-chart"),this.ctrl=n}},{key:"onInitEditMode",value:function(){this.addEditorTab("Grouping","public/plugins/opennms-helm-app/panels/alarm-histogram/editor.html",2)}},{key:"onDataReceived",value:function(t){switch(this.panel.groupProperty){case"acknowledged":var e=i.a.countBy(this.query(t,"Acked By"),(function(t){return i.a.isNil(t).toString()}));this.series=[{name:"Outstanding",count:i.a.defaultTo(e.true,0),color:this.scope.$root.colors[0]},{name:"Acknowledged",count:i.a.defaultTo(e.false,0),color:this.scope.$root.colors[4]}];break;case"severity":var r=i.a.countBy(this.query(t,"Severity"));this.series=[{name:"Cleared",count:i.a.defaultTo(r.CLEARED,0),color:"#EEE000"},{name:"Normal",count:i.a.defaultTo(r.NORMAL,0),color:"#86B15B"},{name:"Indeterm.",count:i.a.defaultTo(r.INDETERMINATE,0),color:"#990000"},{name:"Warning",count:i.a.defaultTo(r.WARNING,0),color:"#FCCC3B"},{name:"Minor",count:i.a.defaultTo(r.MINOR,0),color:"#EE901C"},{name:"Major",count:i.a.defaultTo(r.MAJOR,0),color:"#E3692F"},{name:"Critical",count:i.a.defaultTo(r.CRITICAL,0),color:"#DB4345"}]}this.render()}},{key:"onDataError",value:function(){this.series=[],this.render()}},{key:"onRender",value:function(){var t=this,e=this.ctrl.height||this.ctrl.panel.height||this.ctrl.row&&this.ctrl.row.height;if(i.a.isString(e)&&(e=parseInt(e.replace("px",""),10)),0===this.elem.width()||0===e||void 0===e)return!(this._renderRetries>this.retryTimes||(this._renderRetries++,this.$timeout((function(){t.onRender()}),this.retryDelay),0));switch(e-=5,e-=this.ctrl.panel.title?24:9,this.elem.css("height",e+"px"),this.panel.direction){case"horizontal":u.a.plot(this.elem,i.a.map(this.series,(function(t){return{data:[[t.count,t.name]],color:t.color}})),{series:{bars:{show:!0,barWidth:.6,align:"center",fill:.8,lineWidth:1,horizontal:!0}},yaxis:{mode:"categories",tickLength:0,autoscaleMargin:.02},grid:{borderWidth:0}});break;case"vertical":u.a.plot(this.elem,i.a.map(this.series,(function(t){return{data:[[t.name,t.count]],color:t.color}})),{series:{bars:{show:!0,barWidth:.5,align:"center",fill:.8,lineWidth:1,horizontal:!1}},xaxis:{mode:"categories",tickLength:0,autoscaleMargin:.02},grid:{borderWidth:0}})}this.ctrl.renderingCompleted()}},{key:"query",value:function(t,e){for(var r=[],n=0;n<t.length;n++)for(var o=i.a.findIndex(t[n].columns,{text:e}),a=t[n]&&t[n].rows?t[n].rows:[],u=0;u<a.length;u++)r.push(t[n].rows[u][o]);return r}}])&&c(e.prototype,r),n&&c(e,n),Object.defineProperty(e,"prototype",{writable:!1}),a}(n.MetricsPanelCtrl);v.templateUrl="public/plugins/opennms-helm-app/panels/alarm-histogram/module.html";var m=r(3);r.d(e,"AlarmHistogramCtrl",(function(){return v})),r.d(e,"PanelCtrl",(function(){return v})),Object(m.e)()},6:function(t,r){t.exports=e},8:function(t,e){t.exports=r},9:function(t,e){t.exports=n}})}));
//# sourceMappingURL=module.js.map