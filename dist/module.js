define(["@grafana/data","react","@grafana/ui","@grafana/runtime","@emotion/css","rxjs"],((e,n,t,r,a,o)=>(()=>{"use strict";var i={6089:e=>{e.exports=a},7781:n=>{n.exports=e},8531:e=>{e.exports=r},2007:e=>{e.exports=t},5959:e=>{e.exports=n},1269:e=>{e.exports=o}},l={};function c(e){var n=l[e];if(void 0!==n)return n.exports;var t=l[e]={exports:{}};return i[e](t,t.exports,c),t.exports}c.n=e=>{var n=e&&e.__esModule?()=>e.default:()=>e;return c.d(n,{a:n}),n},c.d=(e,n)=>{for(var t in n)c.o(n,t)&&!c.o(e,t)&&Object.defineProperty(e,t,{enumerable:!0,get:n[t]})},c.o=(e,n)=>Object.prototype.hasOwnProperty.call(e,n),c.r=e=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})};var u={};return(()=>{c.r(u),c.d(u,{plugin:()=>f});var e=c(7781),n=c(5959),t=c.n(n);class r extends n.PureComponent{render(){return n.createElement("div",{className:"page-container"},"OpenNMS Plugin for Grafana")}}var a=c(2007),o=c(8531),i=c(6089),l=c(1269);function s(e,n,t,r,a,o,i){try{var l=e[o](i),c=l.value}catch(e){return void t(e)}l.done?n(c):Promise.resolve(c).then(r,a)}function d(e){return function(){var n=this,t=arguments;return new Promise((function(r,a){var o=e.apply(n,t);function i(e){s(o,r,a,i,l,"next",e)}function l(e){s(o,r,a,i,l,"throw",e)}i(void 0)}))}}const p=e=>({colorWeak:i.css`
    color: ${e.colors.text.secondary};
  `,marginTop:i.css`
    margin-top: ${e.spacing(3)};
  `}),g=function(){var e=d((function*(e,n){try{yield m(e,n),window.location.reload()}catch(e){console.error("Error while updating the plugin",e)}}));return function(n,t){return e.apply(this,arguments)}}(),m=function(){var e=d((function*(e,n){const t=(0,o.getBackendSrv)().fetch({url:`/api/plugins/${e}/settings`,method:"POST",data:n});return(0,l.lastValueFrom)(t)}));return function(n,t){return e.apply(this,arguments)}}(),f=(new e.AppPlugin).setRootPage(r).addConfigPage({title:"Configuration",icon:"cog",body:({plugin:e})=>{const n=(0,a.useStyles2)(p),{enabled:r,jsonData:o}=e.meta;return t().createElement("div",{className:"gf-form-group"},t().createElement("div",null,t().createElement(a.Legend,null,"Enable / Disable"),!r&&t().createElement(t().Fragment,null,t().createElement("div",{className:n.colorWeak},"The plugin is currently not enabled."),t().createElement(a.Button,{className:n.marginTop,variant:"primary",onClick:()=>g(e.meta.id,{enabled:!0,pinned:!0,jsonData:o})},"Enable plugin")),r&&t().createElement(t().Fragment,null,t().createElement("div",{className:n.colorWeak},"The plugin is currently enabled."),t().createElement(a.Button,{className:n.marginTop,variant:"destructive",onClick:()=>g(e.meta.id,{enabled:!1,pinned:!1,jsonData:o})},"Disable plugin"))))},id:"configuration"})})(),u})()));
//# sourceMappingURL=module.js.map