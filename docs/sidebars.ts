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
    'intro',
    {
      type: 'category',
      label: '入門ガイド',
      items: [
        'getting-started/installation',
        'getting-started/quick-start',
        'getting-started/basic-syntax',
      ],
    },
    {
      type: 'category',
      label: 'チュートリアル基本',
      items: [
        'tutorial-basics/create-a-document',
        'tutorial-basics/create-a-blog-post',
        'tutorial-basics/create-a-page',
        'tutorial-basics/markdown-features',
        'tutorial-basics/deploy-your-site',
        'tutorial-basics/congratulations',
      ],
    },
    {
      type: 'category',
      label: 'チュートリアル応用',
      items: [
        'tutorial-extras/manage-docs-versions',
        'tutorial-extras/translate-your-site',
      ],
    },
  ],
  referenceSidebar: [
    'reference/intro',
  ],
};

export default sidebars;
