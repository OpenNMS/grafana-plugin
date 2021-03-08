define(["lodash","app/plugins/sdk","angular","@grafana/ui","@grafana/data","app/core/app_events"],(function(e,t,r,n,a,o){return function(e){var t={};function r(n){if(t[n])return t[n].exports;var a=t[n]={i:n,l:!1,exports:{}};return e[n].call(a.exports,a,a.exports,r),a.l=!0,a.exports}return r.m=e,r.c=t,r.d=function(e,t,n){r.o(e,t)||Object.defineProperty(e,t,{enumerable:!0,get:n})},r.r=function(e){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},r.t=function(e,t){if(1&t&&(e=r(e)),8&t)return e;if(4&t&&"object"==typeof e&&e&&e.__esModule)return e;var n=Object.create(null);if(r.r(n),Object.defineProperty(n,"default",{enumerable:!0,value:e}),2&t&&"string"!=typeof e)for(var a in e)r.d(n,a,function(t){return e[t]}.bind(null,a));return n},r.n=function(e){var t=e&&e.__esModule?function(){return e.default}:function(){return e};return r.d(t,"a",t),t},r.o=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},r.p="",r(r.s=58)}([function(t,r){t.exports=e},,function(e,t){e.exports=function(e,t){if(!(e instanceof t))throw new TypeError("Cannot call a class as a function")}},function(e,t){function r(e,t){for(var r=0;r<t.length;r++){var n=t[r];n.enumerable=n.enumerable||!1,n.configurable=!0,"value"in n&&(n.writable=!0),Object.defineProperty(e,n.key,n)}}e.exports=function(e,t,n){return t&&r(e.prototype,t),n&&r(e,n),e}},function(e,t){function r(t){return e.exports=r=Object.setPrototypeOf?Object.getPrototypeOf:function(e){return e.__proto__||Object.getPrototypeOf(e)},r(t)}e.exports=r},function(e,t){e.exports=function(e){if(void 0===e)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return e}},function(e,t,r){var n=r(15);e.exports=function(e,t){if("function"!=typeof t&&null!==t)throw new TypeError("Super expression must either be null or a function");e.prototype=Object.create(t&&t.prototype,{constructor:{value:e,writable:!0,configurable:!0}}),t&&n(e,t)}},,function(e,t,r){var n=r(14),a=r(5);e.exports=function(e,t){return!t||"object"!==n(t)&&"function"!=typeof t?a(e):t}},function(e,r){e.exports=t},function(e,t){e.exports=r},,function(e,t){e.exports=n},function(e,t){e.exports=a},function(e,t){function r(t){return"function"==typeof Symbol&&"symbol"==typeof Symbol.iterator?e.exports=r=function(e){return typeof e}:e.exports=r=function(e){return e&&"function"==typeof Symbol&&e.constructor===Symbol&&e!==Symbol.prototype?"symbol":typeof e},r(t)}e.exports=r},function(e,t){function r(t,n){return e.exports=r=Object.setPrototypeOf||function(e,t){return e.__proto__=t,e},r(t,n)}e.exports=r},,,function(e,t,r){"use strict";r.d(t,"a",(function(){return m}));var n=r(20),a=r.n(n),o=r(2),i=r.n(o),u=r(3),s=r.n(u),l=r(27),c=r.n(l);function f(e,t){var r;if("undefined"==typeof Symbol||null==e[Symbol.iterator]){if(Array.isArray(e)||(r=function(e,t){if(!e)return;if("string"==typeof e)return d(e,t);var r=Object.prototype.toString.call(e).slice(8,-1);"Object"===r&&e.constructor&&(r=e.constructor.name);if("Map"===r||"Set"===r)return Array.from(e);if("Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r))return d(e,t)}(e))||t&&e&&"number"==typeof e.length){r&&(e=r);var n=0,a=function(){};return{s:a,n:function(){return n>=e.length?{done:!0}:{done:!1,value:e[n++]}},e:function(e){throw e},f:a}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}var o,i=!0,u=!1;return{s:function(){r=e[Symbol.iterator]()},n:function(){var e=r.next();return i=e.done,e},e:function(e){u=!0,o=e},f:function(){try{i||null==r.return||r.return()}finally{if(u)throw o}}}}function d(e,t){(null==t||t>e.length)&&(t=e.length);for(var r=0,n=new Array(t);r<t;r++)n[r]=e[r];return n}var h=/\s*,\s*/,p=function(e){return"string"==typeof e||e instanceof String},v=function(e){if(e&&Array.isArray(e)&&e.length>0)return e[e.length-1]},m=function(){function e(){i()(this,e)}return s()(e,null,[{key:"parenthesize",value:function(t){return e._process(c()(t,{brackets:["()"]}))}},{key:"parenthesizeWithArguments",value:function(t){return e.parenthesize(t).map((function(t){return t&&t.arguments&&(t.arguments.length<2?t.arguments=e.getArguments(t.arguments[0]):console.log("unexpected arguments, expected a single string:",t)),t}))}},{key:"findFunctions",value:function(t){return e.parenthesizeWithArguments(t).filter((function(e){return e&&void 0!==e.name}))}},{key:"getArguments",value:function(e){var t=null==e?"":e;if(0===t.length)return[];var r=t.split(h);return Array.isArray(r)?r:[r]}},{key:"replace",value:function(t,r){var n=e.parenthesizeWithArguments(t),a="";return n.forEach((function(e){p(e)?a+=e:e.name?r&&r.hasOwnProperty(e.name)?a+=r[e.name].apply(r[e.name],e.arguments):(a+=e.name+"(",e.arguments&&(a+=e.arguments.join(", ")),a+=")"):console.log("this should not happen... token=",e)})),a}},{key:"format",value:function(t,r){return e.replace(t,{nodeToLabel:function(t){var n=e._getNodeFromCriteria(r,t);return n?n.label:t},resourceToLabel:function(t,n){var a=e._getResource(r,t,n);return a?a.label:n?[t,n].join("."):t},resourceToName:function(t,n){var a=e._getResource(r,t,n);return a?a.name:n?[t,n].join("."):t},resourceToInterface:function(t,n){var a=e._getResource(r,t,n);if(a){var o=a.name.match(/^(\w+)/);if(o||(o=a.label.match(/^(\w+)/)),o)return o[1]}return n?[t,n].join("."):t}})}},{key:"_process",value:function(t){var r=[],n=/^(.*?)(\w+?)\($/,a=!1;return t.forEach((function(o,i){if(a)a=!1;else{var u,s=r.length?r[r.length-1]:void 0,l=t[i+1];if(Array.isArray(o))r.push(e._process(o));else if(null!==(u=n.exec(o))){var c=u[1];c&&c.length>0&&(c.startsWith(")")&&s&&s.name&&(c=c.replace(/^\)/,"")),r.push(c)),r.push({name:u[2],arguments:e._process(l)}),a=!0}else if(p(o)&&o.startsWith(")")&&s&&s.name){var f=o.replace(/^\)/,"");f.length>0&&r.push(f)}else r.push(o)}})),e._flatten(r)}},{key:"_flatten",value:function(t){var r=[];return t.forEach((function(n){if(p(n)){if(0===n.length)return;var a=v(r);p(a)?r[r.length-1]+=n:r.push(n)}else if(n&&n.arguments)n.arguments=e._flatten(n.arguments),r.push(n);else{if(!Array.isArray(n))throw console.log("cannot reach here (args)",n,t),new Error("cannot reach here (args)");var o=e._flatten(n);o.forEach((function(e){var t=v(r);if(p(e)){if(0===e.trim().length)return;p(t)?r[r.length-1]+=e:r.push(e)}else{if(!e||!e.arguments)throw console.log("cannot reach here (result)",t,e,o),new Error("cannot reach here (result)");r.push(e)}}))}})),r}},{key:"_getNodeFromCriteria",value:function(t,r){var n,o,i;if(r&&r.indexOf(":")>0){var u=r.split(":"),s=a()(u,2);o=s[0],i=s[1]}else n=parseInt(r,10);return e._getNodeFromMetadata(t,n,o,i)}},{key:"_getNodeFromMetadata",value:function(e,t,r,n){if(e&&e.nodes){var a=e.nodes.filter((function(e){return void 0!==t&&e.id===t||void 0!==r&&void 0!==n&&e["foreign-source"]===r&&e["foreign-id"]===n}))[0];if(void 0!==a)return a}return null}},{key:"_getResource",value:function(t,r,n){if(void 0===n){var a,o=f(t.resources);try{for(o.s();!(a=o.n()).done;){var i=a.value;if(i.id===r)return i}}catch(e){o.e(e)}finally{o.f()}}else{var u=e._getNodeFromCriteria(t,r);if(u){var s=e._getResourceFromCriteria(t,n,"node["+u["foreign-source"]+":"+u["foreign-id"]+"]","node["+u.id+"]");if(s)return s}}return null}},{key:"_getResourceFromCriteria",value:function(e,t){for(var r=arguments.length,n=new Array(r>2?r-2:0),a=2;a<r;a++)n[a-2]=arguments[a];if(e&&e.resources){var o=e.resources.filter((function(e){if(e.id===t)return!0;var r,a=f(n.map((function(e){return e+"."+t})));try{for(a.s();!(r=a.n()).done;){var o=r.value;if(e.id===o)return!0}}catch(e){a.e(e)}finally{a.f()}return!1}))[0];if(void 0!==o)return o}return null}}]),e}()},,function(e,t,r){var n=r(30),a=r(31),o=r(28),i=r(32);e.exports=function(e,t){return n(e)||a(e,t)||o(e,t)||i()}},,,,,function(e,t){e.exports=function(e,t){(null==t||t>e.length)&&(t=e.length);for(var r=0,n=new Array(t);r<t;r++)n[r]=e[r];return n}},,function(e,t,r){"use strict";function n(e,t){if("string"!=typeof e)return[e];var r=[e];"string"==typeof t||Array.isArray(t)?t={brackets:t}:t||(t={});var n=t.brackets?Array.isArray(t.brackets)?t.brackets:[t.brackets]:["{}","[]","()"],a=t.escape||"___",o=!!t.flat;n.forEach((function(e){var t=new RegExp(["\\",e[0],"[^\\",e[0],"\\",e[1],"]*\\",e[1]].join("")),n=[];function o(t,o,i){var u=r.push(t.slice(e[0].length,-e[1].length))-1;return n.push(u),a+u+a}r.forEach((function(e,n){for(var a,i=0;e!=a;)if(a=e,e=e.replace(t,o),i++>1e4)throw Error("References have circular dependency. Please, check them.");r[n]=e})),n=n.reverse(),r=r.map((function(t){return n.forEach((function(r){t=t.replace(new RegExp("(\\"+a+r+"\\"+a+")","g"),e[0]+"$1"+e[1])})),t}))}));var i=new RegExp("\\"+a+"([0-9]+)\\"+a);return o?r:function e(t,r,n){for(var a,o=[],u=0;a=i.exec(t);){if(u++>1e4)throw Error("Circular references in parenthesis");o.push(t.slice(0,a.index)),o.push(e(r[a[1]],r)),t=t.slice(a.index+a[0].length)}return o.push(t),o}(r[0],r)}function a(e,t){if(t&&t.flat){var r,n=t&&t.escape||"___",a=e[0];if(!a)return"";for(var o=new RegExp("\\"+n+"([0-9]+)\\"+n),i=0;a!=r;){if(i++>1e4)throw Error("Circular references in "+e);r=a,a=a.replace(o,u)}return a}return e.reduce((function e(t,r){return Array.isArray(r)&&(r=r.reduce(e,"")),t+r}),"");function u(t,r){if(null==e[r])throw Error("Reference "+r+"is undefined");return e[r]}}function o(e,t){return Array.isArray(e)?a(e,t):n(e,t)}o.parse=n,o.stringify=a,e.exports=o},function(e,t,r){var n=r(25);e.exports=function(e,t){if(e){if("string"==typeof e)return n(e,t);var r=Object.prototype.toString.call(e).slice(8,-1);return"Object"===r&&e.constructor&&(r=e.constructor.name),"Map"===r||"Set"===r?Array.from(e):"Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r)?n(e,t):void 0}}},,function(e,t){e.exports=function(e){if(Array.isArray(e))return e}},function(e,t){e.exports=function(e,t){if("undefined"!=typeof Symbol&&Symbol.iterator in Object(e)){var r=[],n=!0,a=!1,o=void 0;try{for(var i,u=e[Symbol.iterator]();!(n=(i=u.next()).done)&&(r.push(i.value),!t||r.length!==t);n=!0);}catch(e){a=!0,o=e}finally{try{n||null==u.return||u.return()}finally{if(a)throw o}}return r}}},function(e,t){e.exports=function(){throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}},function(e,t,r){"use strict";var n=r(2),a=r.n(n);"undefined"!=typeof angular?angular.module("grafana.directives").directive("onmsTimeoutSettings",(function(){return{templateUrl:"public/plugins/opennms-helm-app/components/timeout.html",restrict:"E",controller:"OnmsTimeoutCtrl",controllerAs:"ctrl",bindToController:!0,scope:{current:"="}}})).controller("OnmsTimeoutCtrl",(function e(){a()(this,e),this.current||console.log("no current controller!"),this.current.jsonData.timeout||(this.current.jsonData.timeout=10),this.patterns={timeout:/^\d+$/}})):console.warn("Angular not found!  <onms-timeout> will not work!")},,,,,,,,,,,,,,,function(e,t){e.exports=o},,,,,,,,,,function(e,t,r){r(12),r(13),r(10),e.exports=r(81)},,,,,,,,,,,,,,,,,,,,,,,function(e,t,r){"use strict";r.r(t),r.d(t,"Datasource",(function(){return y})),r.d(t,"QueryCtrl",(function(){return C})),r.d(t,"ConfigCtrl",(function(){return E})),r.d(t,"QueryOptionsCtrl",(function(){return _}));var n=r(2),a=r.n(n),o=r(20),i=r.n(o),u=r(3),s=r.n(u),l=Object.freeze({Attribute:"attribute",Expression:"expression",Filter:"filter"}),c=r(0),f=r.n(c);function d(e,t){return!f.a.isNull(e)&&!f.a.isEmpty(e)&&e.indexOf("$"+t)>=0}function h(e,t){if(f.a.isNull(e)||f.a.isEmpty(e))return e;var r=e;return f.a.each(t,(function(e){var t="\\$"+e.name.replace(/[.*+?^${}()|[\]\\]/g,"\\$&");r=r.replace(new RegExp(t,"g"),e.value)})),r}function p(e,t,r,n,a,o){void 0===n&&(n=function(){}),void 0===a&&(a=d),void 0===o&&(o=h);var i=f.a.clone(r);i.push({name:"index",value:[0]});var u=[];if(f.a.each(i,(function(r){f.a.find(t,(function(t){return a(e[t],r.name)}))&&u.push(r)})),u.length<1)return n(e),[e];var s=function(e){var t=[];f.a.each(e,(function(e){t.push(e.value)}));var r,n,a,o=(n=[],a=(r=t).length-1,function e(t,o){for(var i=0,u=r[o].length;i<u;i++){var s=t.slice(0);s.push(r[o][i]),o===a?n.push(s):e(s,o+1)}}([],0),n),i=[];return f.a.each(o,(function(t){for(var r=[],n=0,a=e.length;n<a;n++){var o=JSON.parse(JSON.stringify(e[n]));o.value=t[n],r.push(o)}i.push(r)})),i}(u),l=[],c=0;return f.a.each(s,(function(r){f.a.each(r,(function(e){"index"===e.name&&(e.value="idx"+c,c+=1)}));var a=f.a.clone(e);f.a.each(t,(function(e){a[e]=o(a[e],r)})),n(a),l.push(a)})),l}var v=r(18);function m(e,t){var r;if("undefined"==typeof Symbol||null==e[Symbol.iterator]){if(Array.isArray(e)||(r=function(e,t){if(!e)return;if("string"==typeof e)return g(e,t);var r=Object.prototype.toString.call(e).slice(8,-1);"Object"===r&&e.constructor&&(r=e.constructor.name);if("Map"===r||"Set"===r)return Array.from(e);if("Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r))return g(e,t)}(e))||t&&e&&"number"==typeof e.length){r&&(e=r);var n=0,a=function(){};return{s:a,n:function(){return n>=e.length?{done:!0}:{done:!1,value:e[n++]}},e:function(e){throw e},f:a}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}var o,i=!0,u=!1;return{s:function(){r=e[Symbol.iterator]()},n:function(){var e=r.next();return i=e.done,e},e:function(e){u=!0,o=e},f:function(){try{i||null==r.return||r.return()}finally{if(u)throw o}}}}function g(e,t){(null==t||t>e.length)&&(t=e.length);for(var r=0,n=new Array(t);r<t;r++)n[r]=e[r];return n}var y=function(){function e(t,r,n,o){a()(this,e),this.type=t.type,this.url=t.url,this.name=t.name,this.basicAuth=t.basicAuth,this.withCredentials=t.withCredentials,this.interval=(t.jsonData||{}).timeInterval,t.jsonData&&t.jsonData.timeout&&(this.timeout=1e3*parseInt(t.jsonData.timeout,10)),this.$q=r,this.backendSrv=n,this.templateSrv=o,this.searchLimit=25,this.target={}}return e.$inject=["instanceSettings","$q","backendSrv","templateSrv"],s()(e,[{key:"doOpenNMSRequest",value:function(e){return(this.basicAuth||this.withCredentials)&&(e.withCredentials=!0),this.basicAuth&&(e.headers=e.headers||{},e.headers.Authorization=this.basicAuth),e.url=this.url+e.url,this.timeout&&(e.timeout=this.timeout),this.backendSrv.datasourceRequest(e)}},{key:"decorateError",value:function(e){var t=e;e.err&&(t=e.err);var r=t.statusText||"Request failed.";return t.cancelled&&(delete t.cancelled,r="Request timed out."),e.cancelled&&(delete e.cancelled,r="Request timed out."),t.message||(t.message=r),t.status||(t.status="error"),t}},{key:"query",value:function(t){var r=this,n=this.buildQuery(t),a=i()(n,2),o=a[0],u=a[1];return o.source.length>0?this.doOpenNMSRequest({url:"/rest/measurements",data:o,method:"POST",headers:{"Content-Type":"application/json"}}).then((function(t){return t.status<200||t.status>=300?(console.warn("Response code:",t),r.$q.reject(t)):e.processMeasurementsResponse(t)})).then((function(e){return e.data=f.a.sortBy(e.data,(function(e){return f.a.indexOf(u,e.label)})),e})).catch((function(e){return r.$q.reject(r.decorateError(e))})):this.$q.when({data:[]})}},{key:"testDatasource",value:function(){var e=this;return this.doOpenNMSRequest({url:"/rest/info",method:"GET"}).then((function(e){return 200===e.status?{status:"success",message:"Data source is working",title:"Success"}:{status:"danger",message:"OpenNMS provided a response, but no metadata was found.",title:"Unexpected Response "+e.status}})).catch((function(t){return e.decorateError(t)}))}},{key:"metricFindQuery",value:function(e){if(null==e||""===e)return this.$q.resolve([]);var t=f.a.first(this.interpolateValue(e));if(void 0!==t){var r,n=m(v.a.findFunctions(t));try{for(n.s();!(r=n.n()).done;){var a=r.value;if("nodeFilter"===a.name)return this.metricFindNodeFilterQuery.apply(this,a.arguments);if("nodeResources"===a.name)return this.metricFindNodeResourceQuery.apply(this,a.arguments);console.warn("Unknown function in interpolated query: "+t,a)}}catch(e){n.e(e)}finally{n.f()}}return this.$q.resolve([])}},{key:"metricFindNodeFilterQuery",value:function(e){return this.doOpenNMSRequest({url:"/rest/nodes",method:"GET",params:{filterRule:e,limit:0}}).then((function(e){e.data.count>e.data.totalCount&&console.warn("Filter matches "+e.data.totalCount+" records, but only "+e.data.count+" will be used.");var t=[];return f.a.each(e.data.node,(function(e){var r=e.id.toString();null!==e.foreignId&&null!==e.foreignSource&&(r=e.foreignSource+":"+e.foreignId),t.push({text:e.label,value:r,expandable:!0})})),t}))}},{key:"metricFindNodeResourceQuery",value:function(t){var r="id",n="*";return(arguments.length<=1?0:arguments.length-1)>0&&(r=arguments.length<=1?void 0:arguments[1]),(arguments.length<=1?0:arguments.length-1)>1&&(n=arguments.length<=2?void 0:arguments[2]),this.doOpenNMSRequest({url:"/rest/resources/"+encodeURIComponent(e.getNodeResource(t)),method:"GET",params:{depth:1}}).then((function(e){var t=[];return f.a.each(e.data.children.resource,(function(e){var a,o=e.id.match(/node(Source)?\[.*?\]\.(.*)/);switch(r){case"id":a=o[2];break;case"label":a=e.label;break;case"name":a=e.name;break;default:a=o[2],console.warn("Unknown resource property '".concat(r,"' specified. Using 'id' instead."))}("*"===n&&o||0===o[2].indexOf(n+"["))&&t.push({text:a,value:o[2],expandable:!0})})),t}))}},{key:"buildQuery",value:function(t){var r=t.maxDataPoints||300,n=t.intervalMs||6e4,a=this,o=t.range.from.valueOf(),i=t.range.to.valueOf(),u=Math.floor((i-o)/r),s={start:o,end:i,step:u=u<n?n:u,relaxed:!0,maxrows:r,source:[],expression:[]},c=[];return f.a.each(t.targets,(function(r){var n="false";if(r.hide&&(n=!0),r.type===l.Attribute){if(!(r.nodeId&&r.resourceId&&r.attribute))return;var o=r.label;void 0!==o&&""!==o||(o=r.attribute);var i={aggregation:r.aggregation,attribute:r.attribute,label:o,resourceId:r.resourceId,nodeId:r.nodeId,transient:n};void 0!==r.subattribute&&""!==r.subattribute&&(i.datasource=r.subattribute),void 0!==r.fallbackAttribute&&""!==r.fallbackAttribute&&(i["fallback-attribute"]=r.fallbackAttribute),i=a.interpolateSourceVariables(i,t.scopedVars,(function(t){t.resourceId=e.getRemoteResourceId(t.nodeId,t.resourceId),delete t.nodeId})),s.source=s.source.concat(i),c=c.concat(f.a.map(i,"label"))}else if(r.type===l.Expression){if(!r.label||!r.expression)return;var u={label:r.label,value:r.expression,transient:n};u=a.interpolateExpressionVariables(u,t.scopedVars),s.expression=s.expression.concat(u),c=c.concat(f.a.map(u,"label"))}else if(r.type===l.Filter){if(!r.filter)return;var d=a.interpolateVariables(r.filterParameters,f.a.keys(r.filterParameters),t.scopedVars),h=f.a.map(d,(function(e){var t=[];return f.a.each(e,(function(e,r){void 0!==e&&""!==e&&null!==e&&t.push({key:r,value:e})})),{name:r.filter.name,parameter:t}}));s.filter?s.filter=s.filter.concat(h):s.filter=h}})),[s,c]}},{key:"interpolateSourceVariables",value:function(e,t,r){return this.interpolateVariables(e,["nodeId","resourceId","attribute","datasource","label"],t,r)}},{key:"interpolateExpressionVariables",value:function(e,t){return this.interpolateVariables(e,["value","label"],t)}},{key:"interpolateValue",value:function(e,t){return f.a.map(this.interpolateVariables({value:e},["value"],t),(function(e){return e.value}))}},{key:"interpolateVariables",value:function(e,t,r,n){var a=[];return f.a.each(this.templateSrv.variables,(function(e){var t={name:e.name,value:[]};r&&void 0!==r[t.name]?t.value.push(r[t.name].value):f.a.isString(e.current.value)?t.value.push(e.current.value):f.a.each(e.current.value,(function(r){"$__all"===r?f.a.each(e.options,(function(e){"$__all"!==e.value&&t.value.push(e.value)})):t.value.push(r)})),a.push(t)})),p(e,t,a,n)}},{key:"searchForNodes",value:function(e,t){return this.doOpenNMSRequest({url:"/rest/nodes",method:"GET",params:{offset:t,limit:this.searchLimit,match:"any",comparator:"ilike",orderBy:"id",order:"asc",label:"%"+e+"%",sysName:"%"+e+"%","ipInterface.ipAddress":"%"+e+"%","ipInterface.ipHostName":"%"+e+"%",foreignId:e+"%"}})}},{key:"getResourcesWithAttributesForNode",value:function(t){var r=f.a.first(this.interpolateValue(t));return this.doOpenNMSRequest({url:"/rest/resources/fornode/"+encodeURIComponent(r),method:"GET",params:{depth:-1}}).then((function(t){return e.flattenResourcesWithAttributes([t.data],[])}))}},{key:"getAvailableFilters",value:function(){return this.doOpenNMSRequest({url:"/rest/measurements/filters",method:"GET"})}},{key:"suggestAttributes",value:function(t,r,n){var a=f.a.first(this.interpolateValue(t)),o=f.a.first(this.interpolateValue(r)),i=e.getRemoteResourceId(a,o);return this.doOpenNMSRequest({url:"/rest/resources/"+encodeURIComponent(i),method:"GET",params:{depth:-1}}).then((function(e){n=n.toLowerCase();var t=[];return f.a.each(e.data.rrdGraphAttributes,(function(e,r){r.toLowerCase().indexOf(n)>=0&&t.push(r)})),t.sort(),t}))}}],[{key:"processMeasurementsResponse",value:function(e){var t,r,n,a,o,i,u,s=e.data.labels,l=e.data.columns,c=e.data.timestamps,f=e.data.metadata,d=[];if(void 0!==c)for(n=c.length,a=l.length,t=0;t<a;t++){for(u=!1,o=[],r=0;r<n;r++)c[r]<e.data.start||c[r]>e.data.end||("NaN"===(i=l[t].values[r])&&(i=null),u||isNaN(i)||(u=!0),o.push([i,c[r]]));var h=s[t];f&&f.resources&&(h=v.a.format(h,f)),u&&d.push({target:h,label:s[t],datapoints:o})}return{data:d}}},{key:"flattenResourcesWithAttributes",value:function(t,r){return f.a.each(t,(function(t){void 0!==t.rrdGraphAttributes&&Object.keys(t.rrdGraphAttributes).length>0&&r.push(t),void 0!==t.children&&t.children.resource.length>0&&e.flattenResourcesWithAttributes(t.children.resource,r)})),r}},{key:"getNodeResource",value:function(e){return(e.indexOf(":")>0?"nodeSource[":"node[")+e+"]"}},{key:"getRemoteResourceId",value:function(t,r){return e.getNodeResource(t)+"."+r}}]),e}(),b=r(6),w=r.n(b),S=r(8),k=r.n(S),x=r(4),R=r.n(x),A=function(){function e(t){a()(this,e),this.$scope=t,this.query="",this.offset=0,this.selectedRow=null,this.currentPage=0,this.pageSize=25,this.selfPagination=!1,this.searchForRows(),$('[name="perf-modal-query"]').select()}return e.$inject=["$scope"],s()(e,[{key:"searchForRows",value:function(){var e=this;this.searching=!0,this.$scope.search(this.query,this.offset).then((function(t){e.selectedRow=null,e.allResult=t.rows,t.rows.length>e.pageSize?(e.selfPagination=!0,e.count=0,e.updateSelfPageinatedData()):(e.rows=t.rows,e.count=(t.count?t.count:e.rows.length)+e.offset),e.totalCount=t.totalCount,e.numberOfPages=Math.ceil(e.totalCount/e.pageSize)})).finally((function(){e.$scope.$evalAsync((function(){e.searching=!1}))}))}},{key:"updateSelfPageinatedData",value:function(){this.rows=this.allResult.slice(this.offset,this.offset+this.pageSize),this.count+=this.rows.length}},{key:"startFrom",value:function(e,t){return e*t}},{key:"setClickedRow",value:function(e){this.selectedRow===e?this.selectedRow=null:(this.selectedRow=e,this.row=this.rows[this.selectedRow])}},{key:"nextPage",value:function(){this.currentPage+1!=this.$scope.ctrl.numberOfPages&&(this.offset+=this.pageSize,this.currentPage++,this.selfPagination?this.updateSelfPageinatedData():this.searchForRows())}},{key:"prevPage",value:function(){0!=this.currentPage&&(this.offset-=this.pageSize,this.currentPage--,this.searchForRows())}},{key:"cancel",value:function(){this.$scope.result.reject(),this.$scope.dismiss()}},{key:"ok",value:function(){null!==this.selectedRow?this.$scope.result.resolve(this.row):this.$scope.result.reject(),this.$scope.dismiss()}}]),e}();"undefined"!=typeof angular&&angular.module("grafana.controllers").controller("OpenNMSModalSelectionCtrl",A);var O=r(9),I=r(48),j=r.n(I);function N(e){var t=function(){if("undefined"==typeof Reflect||!Reflect.construct)return!1;if(Reflect.construct.sham)return!1;if("function"==typeof Proxy)return!0;try{return Date.prototype.toString.call(Reflect.construct(Date,[],(function(){}))),!0}catch(e){return!1}}();return function(){var r,n=R()(e);if(t){var a=R()(this).constructor;r=Reflect.construct(n,arguments,a)}else r=n.apply(this,arguments);return k()(this,r)}}var C=function(e){r.$inject=["$rootScope","$scope","$injector","$q","$modal"],w()(r,e);var t=N(r);function r(e,n,o,i,u){var s;return a()(this,r),(s=t.call(this,n,o)).types=l,s.error=s.validateTarget(),s.$rootScope=e,s.$q=i,s.$modal=u,s}return s()(r,[{key:"openNodeSelectionModal",value:function(){var e=this;this.showSelectionModal("nodes",{"#":"id",Label:"label","Foreign ID":"foreignId",sysName:"sysName"},(function(t,r){return e.datasource.searchForNodes(t,r).then((function(e){return{count:e.data.count,totalCount:e.data.totalCount,rows:e.data.node}}))}),(function(t){f.a.isUndefined(t.foreignId)||f.a.isNull(t.foreignId)||f.a.isUndefined(t.foreignSource)||f.a.isNull(t.foreignSource)?e.target.nodeId=t.id:e.target.nodeId=t.foreignSource+":"+t.foreignId,e.targetBlur("nodeId")}))}},{key:"openResourceSelectionModal",value:function(){var e=this;function t(e,t){var r=e;return t.length>=1&&(t=t.toLowerCase(),r=f.a.filter(e,(function(e){return e.key.indexOf(t)>=0}))),{totalCount:r.length,rows:r}}e.nodeResources=void 0,this.showSelectionModal("resources",{Label:"label",Name:"name"},(function(r){if(void 0!==e.nodeResources){var n=e.$q.defer();return n.resolve(t(e.nodeResources,r)),n.promise}return e.datasource.getResourcesWithAttributesForNode(e.target.nodeId).then((function(n){return f.a.each(n,(function(e){e.key=e.label.toLowerCase()+e.name.toLowerCase()})),e.nodeResources=f.a.sortBy(n,(function(e){return e.label})),t(e.nodeResources,r)}))}),(function(t){var r=/node(Source)?\[.*?]\.(.*)$/.exec(t.id);e.target.resourceId=r[2],e.targetBlur("resourceId")}))}},{key:"openAttributeSelectionModal",value:function(e){var t=this;e||(e="attribute"),this.showSelectionModal("attributes",{Name:"name"},(function(e){return t.datasource.suggestAttributes(t.target.nodeId,t.target.resourceId,e).then((function(e){var t=[];return f.a.each(e,(function(e){t.push({name:e})})),{totalCount:t.length,rows:t}}))}),(function(r){t.target[e]=r.name,t.targetBlur(e)}))}},{key:"openFilterSelectionModal",value:function(){var e=this;this.showSelectionModal("filters",{Name:"name",Description:"description",Backend:"backend"},(function(){return e.datasource.getAvailableFilters().then((function(e){return{totalCount:e.data.length,rows:e.data}}))}),(function(t){e.target.filter=t,e.targetBlur("filter")}))}},{key:"showSelectionModal",value:function(e,t,r,n){var a=this.$rootScope.$new();a.label=e,a.columns=t,a.search=r,a.result=this.$q.defer(),a.result.promise.then(n);var o=this.$modal({template:"public/plugins/opennms-helm-app/datasources/perf-ds/partials/modal.selection.html",persist:!1,show:!1,scope:a,keyboard:!1});return this.$q.when(o).then((function(e){return e.modal("show")}))}},{key:"targetBlur",value:function(e,t){void 0===t&&(t=!0);var r=this.validateTarget(e,t);r?(j.a.emit("alert-error",["Error",r]),this.error=r):this.refresh()}},{key:"validateTarget",value:function(e,t){if(this.target.type===l.Attribute||this.target.type===l.Expression){var r={nodeId:"You must supply a node id.",resourceId:"You must supply a resource id.",attribute:"You must supply an attribute.",expression:"You must supply an expression.",label:"You must supply a label."};if(t&&e in r&&!this.target[e])return r[e];if(t&&!this.target[e])return e+" is a required field."}else if(this.target.type===l.Filter)if("filterName"===e){if(!this.target.filter||this.target.filter&&(!this.target.filter.name||0===this.target.filter.name.trim().length))return"You must select a filter."}else if("type"!==e&&t&&(!this.target.filterParameters||!(e in this.target.filterParameters)||!this.target.filterParameters[e]))return e+" is a required field.";return null}},{key:"getCollapsedText",value:function(){return this.target.type===l.Attribute?"Attribute: "+this.target.attribute:this.target.type===l.Expression?"Expression: "+this.target.label:this.target.type===l.Filter?"Filter: "+this.target.filter.name:"<Incomplete>"}}]),r}(O.QueryCtrl);C.templateUrl="datasources/perf-ds/partials/query.editor.html";r(33);var E=function e(){a()(this,e)};E.templateUrl="datasources/perf-ds/partials/config.html";var _=function e(){a()(this,e)};_.templateUrl="datasources/perf-ds/partials/query.options.html",Object(O.loadPluginCss)({dark:"plugins/opennms-helm-app/datasources/perf-ds/css/opennms.dark.css",light:"plugins/opennms-helm-app/datasources/perf-ds/css/opennms.light.css"})}])}));
//# sourceMappingURL=module.js.map