import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {
  tutorialSidebar: [
    'tutorial/intro',
    'tutorial/install',
  ],
  referenceSidebar: [
    'reference/intro',
    'reference/compiler',
    'reference/preprocess',
    'reference/lexical_analysis',
    `reference/colony_rule`,
    'reference/expression',
    'reference/condition_kind',
    'reference/type_inference',
    'reference/resource_search',
    'reference/builtin_colony'
  ],
};

export default sidebars;
