"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[837],{8453:(e,n,l)=>{l.d(n,{R:()=>i,x:()=>r});var d=l(6540);const c={},s=d.createContext(c);function i(e){const n=d.useContext(s);return d.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(c):e.components||c:i(e.components),d.createElement(s.Provider,{value:n},e.children)}},9289:(e,n,l)=>{l.r(n),l.d(n,{assets:()=>h,contentTitle:()=>r,default:()=>j,frontMatter:()=>i,metadata:()=>d,toc:()=>o});const d=JSON.parse('{"id":"reference/lexical_analysis","title":"\u5b57\u53e5\u89e3\u6790","description":"\u3053\u3053\u3067\u306f\u3001\u89e3\u6790\u3055\u308c\u308b\u30c8\u30fc\u30af\u30f3\u306e\u4ed5\u69d8\u306b\u3064\u3044\u3066\u8aac\u660e\u3057\u3066\u3044\u307e\u3059\u3002\u305f\u3060\u3057\u3001\u7121\u7528\u306a\u8aac\u660e\u306e\u8907\u96d1\u5316\u3092\u907f\u3051\u308b\u305f\u3081\u3001\u5b9f\u969b\u306b\u306f\u5b57\u53e5\u89e3\u6790\u3060\u3051\u3067\u306a\u304f\u6587\u6cd5\u30ec\u30d9\u30eb\u3067\u5b9f\u88c5\u3092\u884c\u306a\u3063\u3066\u3044\u308b\u5185\u5bb9\u3082\u542b\u307e\u308c\u3066\u3044\u307e\u3059\u3002","source":"@site/docs/reference/lexical_analysis.md","sourceDirName":"reference","slug":"/reference/lexical_analysis","permalink":"/shol/docs/reference/lexical_analysis","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"referenceSidebar","previous":{"title":"\u30d7\u30ea\u30d7\u30ed\u30bb\u30b9","permalink":"/shol/docs/reference/preprocess"},"next":{"title":"\u30b3\u30ed\u30cb\u30fc\u3068\u898f\u5247","permalink":"/shol/docs/reference/colony_rule"}}');var c=l(4848),s=l(8453);const i={},r="\u5b57\u53e5\u89e3\u6790",h={},o=[{value:"\u7a7a\u767d\u6587\u5b57",id:"\u7a7a\u767d\u6587\u5b57",level:2},{value:"\u6539\u884c",id:"\u6539\u884c",level:2},{value:"\u8b58\u5225\u5b50",id:"\u8b58\u5225\u5b50",level:2},{value:"\u30ea\u30c6\u30e9\u30eb",id:"\u30ea\u30c6\u30e9\u30eb",level:2},{value:"bool \u578b\u30ea\u30c6\u30e9\u30eb",id:"bool-\u578b\u30ea\u30c6\u30e9\u30eb",level:3},{value:"double \u578b\u30ea\u30c6\u30e9\u30eb",id:"double-\u578b\u30ea\u30c6\u30e9\u30eb",level:3},{value:"int \u578b\u30ea\u30c6\u30e9\u30eb",id:"int-\u578b\u30ea\u30c6\u30e9\u30eb",level:3},{value:"str \u578b\u30ea\u30c6\u30e9\u30eb",id:"str-\u578b\u30ea\u30c6\u30e9\u30eb",level:3},{value:"symbol \u578b\u30ea\u30c6\u30e9\u30eb",id:"symbol-\u578b\u30ea\u30c6\u30e9\u30eb",level:3},{value:"\u30ad\u30e3\u30d7\u30c1\u30e3",id:"\u30ad\u30e3\u30d7\u30c1\u30e3",level:2},{value:"\u51fa\u529b\u5148",id:"\u51fa\u529b\u5148",level:2},{value:"\u30b3\u30e1\u30f3\u30c8",id:"\u30b3\u30e1\u30f3\u30c8",level:2},{value:"\u6f14\u7b97\u5b50\u30fb\u30de\u30af\u30ed",id:"\u6f14\u7b97\u5b50\u30de\u30af\u30ed",level:2}];function x(e){const n={code:"code",h1:"h1",h2:"h2",h3:"h3",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,s.R)(),...e.components};return(0,c.jsxs)(c.Fragment,{children:[(0,c.jsx)(n.header,{children:(0,c.jsx)(n.h1,{id:"\u5b57\u53e5\u89e3\u6790",children:"\u5b57\u53e5\u89e3\u6790"})}),"\n",(0,c.jsx)(n.p,{children:"\u3053\u3053\u3067\u306f\u3001\u89e3\u6790\u3055\u308c\u308b\u30c8\u30fc\u30af\u30f3\u306e\u4ed5\u69d8\u306b\u3064\u3044\u3066\u8aac\u660e\u3057\u3066\u3044\u307e\u3059\u3002\u305f\u3060\u3057\u3001\u7121\u7528\u306a\u8aac\u660e\u306e\u8907\u96d1\u5316\u3092\u907f\u3051\u308b\u305f\u3081\u3001\u5b9f\u969b\u306b\u306f\u5b57\u53e5\u89e3\u6790\u3060\u3051\u3067\u306a\u304f\u6587\u6cd5\u30ec\u30d9\u30eb\u3067\u5b9f\u88c5\u3092\u884c\u306a\u3063\u3066\u3044\u308b\u5185\u5bb9\u3082\u542b\u307e\u308c\u3066\u3044\u307e\u3059\u3002"}),"\n",(0,c.jsx)(n.h2,{id:"\u7a7a\u767d\u6587\u5b57",children:"\u7a7a\u767d\u6587\u5b57"}),"\n",(0,c.jsx)(n.p,{children:"\u7a7a\u767d\u6587\u5b57\u306f\u7121\u8996\u3055\u308c\u308b\u305f\u3081\u3001\u30ea\u30c6\u30e9\u30eb\u306a\u3069\u306e\u30c8\u30fc\u30af\u30f3\u306e\u9014\u4e2d\u3092\u9664\u304d\u3001\u4efb\u610f\u306e\u4f4d\u7f6e\u306b\u633f\u5165\u3059\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'\u7a7a\u767d\u6587\u5b57 ::= " " | "\\t"\n'})}),"\n",(0,c.jsx)(n.h2,{id:"\u6539\u884c",children:"\u6539\u884c"}),"\n",(0,c.jsx)(n.p,{children:"Shol \u3067\u306f\u3001\u6539\u884c\u3092\u30c8\u30fc\u30af\u30f3\u3068\u3057\u3066\u6271\u3044\u307e\u3059\u3002"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'\u6539\u884c ::= "\\n" | "\\r\\n" | "\\r" | "\\f"\n'})}),"\n",(0,c.jsx)(n.h2,{id:"\u8b58\u5225\u5b50",children:"\u8b58\u5225\u5b50"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'\u8b58\u5225\u5b50 ::= (XID_Start | "_") XID_Continue*\n'})}),"\n",(0,c.jsxs)(n.p,{children:["\u8b58\u5225\u5b50\u306f\u3001Logos \u30af\u30ec\u30fc\u30c8\u306e\u6b63\u898f\u8868\u73fe ",(0,c.jsx)(n.code,{children:"(\\p{XID_Start}|_)\\p{XID_Continue}*"})," \u3067\u5b9f\u88c5\u3057\u3066\u3044\u307e\u3059\u3002\u3053\u308c\u306f Rust \u306e\u8b58\u5225\u5b50\u306e\u5b9a\u7fa9\u306b\u57fa\u3065\u3044\u3066\u304a\u308a\u3001\u65e5\u672c\u8a9e\u306e\u6587\u5b57\u306a\u3069\u3082\u8b58\u5225\u5b50\u3068\u3057\u3066\u4f7f\u7528\u3059\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002"]}),"\n",(0,c.jsx)(n.p,{children:"\u305f\u3060\u3057\u3001\u4ee5\u4e0b\u306e\u4e88\u7d04\u8a9e\u306f\u8b58\u5225\u5b50\u3068\u3057\u3066\u4f7f\u7528\u3059\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u305b\u3093\u3002"}),"\n",(0,c.jsxs)(n.ul,{children:["\n",(0,c.jsx)(n.li,{children:(0,c.jsx)(n.code,{children:"true"})}),"\n",(0,c.jsx)(n.li,{children:(0,c.jsx)(n.code,{children:"false"})}),"\n"]}),"\n",(0,c.jsx)(n.p,{children:"\u8b58\u5225\u5b50\u306f\u3001\u30b3\u30ed\u30cb\u30fc\u540d\u3060\u3051\u3067\u306a\u304f\u3001\u5f0f\u4e2d\u306e\u6587\u5b57\u5217\u30ea\u30c6\u30e9\u30eb\u306e\u4ee3\u308f\u308a\u3068\u3057\u3066\u3082\u4f7f\u7528\u3055\u308c\u307e\u3059\u3002"}),"\n",(0,c.jsx)(n.h2,{id:"\u30ea\u30c6\u30e9\u30eb",children:"\u30ea\u30c6\u30e9\u30eb"}),"\n",(0,c.jsx)(n.h3,{id:"bool-\u578b\u30ea\u30c6\u30e9\u30eb",children:"bool \u578b\u30ea\u30c6\u30e9\u30eb"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'bool\u578b\u30ea\u30c6\u30e9\u30eb ::= "true"\n               | "false"\n'})}),"\n",(0,c.jsx)(n.h3,{id:"double-\u578b\u30ea\u30c6\u30e9\u30eb",children:"double \u578b\u30ea\u30c6\u30e9\u30eb"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'\u5c0f\u6570\u90e8 ::= ("0".."9")+ "."\n        | ("0".."9")* "." ("0".."9")+\n\n\u6307\u6570\u90e8 ::= ("e" | "E") ("+" | "-")? ("0".."9")+\n\ndouble\u578b\u30ea\u30c6\u30e9\u30eb ::= \u5c0f\u6570\u90e8 \u6307\u6570\u90e8?\n                 | ("0".."9")+ \u6307\u6570\u90e8?\n'})}),"\n",(0,c.jsxs)(n.p,{children:["\u4f8b\uff1a",(0,c.jsx)(n.code,{children:"0.1"}),", ",(0,c.jsx)(n.code,{children:"1.0"}),", ",(0,c.jsx)(n.code,{children:"1."}),", ",(0,c.jsx)(n.code,{children:".1"}),", ",(0,c.jsx)(n.code,{children:"1e1"}),", ",(0,c.jsx)(n.code,{children:"1.2e+1"}),", ",(0,c.jsx)(n.code,{children:"1e-5"})]}),"\n",(0,c.jsx)(n.h3,{id:"int-\u578b\u30ea\u30c6\u30e9\u30eb",children:"int \u578b\u30ea\u30c6\u30e9\u30eb"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'int\u578b\u30ea\u30c6\u30e9\u30eb ::= "0"\n              | ("1".."9") ("0".."9")*\n'})}),"\n",(0,c.jsx)(n.h3,{id:"str-\u578b\u30ea\u30c6\u30e9\u30eb",children:"str \u578b\u30ea\u30c6\u30e9\u30eb"}),"\n",(0,c.jsxs)(n.p,{children:[(0,c.jsx)(n.code,{children:'"'})," \u3067\u56f2\u307e\u308c\u305f\u6587\u5b57\u5217\u30ea\u30c6\u30e9\u30eb\u306b\u5bfe\u5fdc\u3057\u307e\u3059\u3002\u4ee5\u4e0b\u306f\u5bfe\u5fdc\u3057\u3066\u3044\u308b\u30a8\u30b9\u30b1\u30fc\u30d7\u30b7\u30fc\u30b1\u30f3\u30b9\u306e\u4e00\u89a7\u3067\u3059\u3002"]}),"\n",(0,c.jsxs)(n.ul,{children:["\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\0"})," : \u30cc\u30eb\u6587\u5b57"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\a"})," : \u30d9\u30eb (\u8b66\u544a)"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\b"})," : \u30d0\u30c3\u30af\u30b9\u30da\u30fc\u30b9"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\t"})," : \u30bf\u30d6"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\n"})," : \u6539\u884c"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\v"})," : \u5782\u76f4\u30bf\u30d6"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\f"})," : \u30d5\u30a9\u30fc\u30e0\u30d5\u30a3\u30fc\u30c9"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\r"})," : \u30ad\u30e3\u30ea\u30c3\u30b8\u30ea\u30bf\u30fc\u30f3"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\e"})," : \u30a8\u30b9\u30b1\u30fc\u30d7"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:'\\"'})," : \u30c0\u30d6\u30eb\u30af\u30a9\u30fc\u30c8"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\\\"})," : \u30d0\u30c3\u30af\u30b9\u30e9\u30c3\u30b7\u30e5"]}),"\n",(0,c.jsxs)(n.li,{children:[(0,c.jsx)(n.code,{children:"\\xXX"}),", ",(0,c.jsx)(n.code,{children:"\\uXXXX"}),", ",(0,c.jsx)(n.code,{children:"\\UXXXXXXXX"})," : 16\u9032\u6570\u8868\u8a18\u306e Unicode \u6587\u5b57 (X: 0-9, A-F, a-f)"]}),"\n"]}),"\n",(0,c.jsx)(n.h3,{id:"symbol-\u578b\u30ea\u30c6\u30e9\u30eb",children:"symbol \u578b\u30ea\u30c6\u30e9\u30eb"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'symbol\u578b\u30ea\u30c6\u30e9\u30eb ::= "\'" \u8b58\u5225\u5b50?\n'})}),"\n",(0,c.jsx)(n.h2,{id:"\u30ad\u30e3\u30d7\u30c1\u30e3",children:"\u30ad\u30e3\u30d7\u30c1\u30e3"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'\u30ad\u30e3\u30d7\u30c1\u30e3 ::= "$" \u8b58\u5225\u5b50?\n'})}),"\n",(0,c.jsx)(n.h2,{id:"\u51fa\u529b\u5148",children:"\u51fa\u529b\u5148"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{children:'\u51fa\u529b\u5148 ::= "#" \u8b58\u5225\u5b50?\n'})}),"\n",(0,c.jsx)(n.h2,{id:"\u30b3\u30e1\u30f3\u30c8",children:"\u30b3\u30e1\u30f3\u30c8"}),"\n",(0,c.jsx)(n.p,{children:"\u30b3\u30e1\u30f3\u30c8\u306f\u30c8\u30fc\u30af\u30f3\u5316\u3055\u308c\u305a\u3001\u7121\u8996\u3055\u308c\u307e\u3059\u3002"}),"\n",(0,c.jsxs)(n.p,{children:["1 \u884c\u306e\u30b3\u30e1\u30f3\u30c8\u3067\u306f ",(0,c.jsx)(n.code,{children:"//"})," \u3092\u30011 \u884c\u4ee5\u4e0a\u306b\u6e21\u308b\u30b3\u30e1\u30f3\u30c8\u306b\u306f ",(0,c.jsx)(n.code,{children:"/*"})," ... ",(0,c.jsx)(n.code,{children:"*/"})," \u306b\u3088\u308b\u30d6\u30ed\u30c3\u30af\u30b3\u30e1\u30f3\u30c8\u3092\u4f7f\u7528\u3067\u304d\u307e\u3059\u3002"]}),"\n",(0,c.jsx)(n.p,{children:"\u30d6\u30ed\u30c3\u30af\u30b3\u30e1\u30f3\u30c8\u306f\u3001\u4ee5\u4e0b\u306e\u3088\u3046\u306b\u30cd\u30b9\u30c8\u3055\u305b\u308b\u3053\u3068\u304c\u53ef\u80fd\u3067\u3059\u3002"}),"\n",(0,c.jsx)(n.pre,{children:(0,c.jsx)(n.code,{className:"language-shol",children:"/*\n\u3053\u3053\u306f\u30b3\u30e1\u30f3\u30c8\n/*\n\u3053\u3053\u3082\u30b3\u30e1\u30f3\u30c8\n*/\n\u3053\u3053\u3082\u30b3\u30e1\u30f3\u30c8\n*/\n"})}),"\n",(0,c.jsx)(n.h2,{id:"\u6f14\u7b97\u5b50\u30de\u30af\u30ed",children:"\u6f14\u7b97\u5b50\u30fb\u30de\u30af\u30ed"}),"\n",(0,c.jsxs)(n.p,{children:[(0,c.jsx)(n.code,{children:":int"}),", ",(0,c.jsx)(n.code,{children:".int"}),", ",(0,c.jsx)(n.code,{children:".abs"})," \u306a\u3069\u306e\u6f14\u7b97\u5b50\u3084\u3001",(0,c.jsx)(n.code,{children:"@debug"})," \u306e\u3088\u3046\u306a\u30de\u30af\u30ed\u306f\u3001\u3059\u3079\u3066 1 \u3064\u306e\u30c8\u30fc\u30af\u30f3\u3068\u3057\u3066\u89e3\u6790\u3055\u308c\u307e\u3059\u3002"]}),"\n",(0,c.jsxs)(n.p,{children:["\u307e\u305f\u3001",(0,c.jsx)(n.code,{children:":int"})," \u3068 ",(0,c.jsx)(n.code,{children:":i"})," \u306e\u3088\u3046\u306a\u7701\u7565\u5f62\u306e\u3042\u308b\u6f14\u7b97\u5b50\u306f\u3001\u3069\u3061\u3089\u3082\u540c\u3058\u30c8\u30fc\u30af\u30f3\u3092\u751f\u6210\u3057\u307e\u3059\u3002"]})]})}function j(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,c.jsx)(n,{...e,children:(0,c.jsx)(x,{...e})}):x(e)}}}]);