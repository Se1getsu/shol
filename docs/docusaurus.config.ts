import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: 'Shol',
  tagline: 'ユニークな独自パラダイムで、新感覚のプログラミング体験',
  favicon: 'img/favicon.ico',

  // Set the production url of your site here
  url: 'https://se1getsu.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/shol/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'Se1getsu', // Usually your GitHub org/user name.
  projectName: 'shol', // Usually your repo name.
  deploymentBranch: 'gh-pages', // デプロイ先のブランチ名
  trailingSlash: false,

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'ja',
    locales: ['ja'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'img/docusaurus-social-card.jpg',
    navbar: {
      title: 'Shol',
      logo: {
        alt: 'Shol Logo',
        src: 'img/logo.png',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'tutorialSidebar',
          position: 'left',
          label: 'チュートリアル',
        },
        {
          type: 'docSidebar',
          sidebarId: 'referenceSidebar',
          position: 'left',
          label: 'リファレンス',
        },
        {
          href: 'https://github.com/Se1getsu/shol',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'ドキュメント',
          items: [
            {
              label: 'チュートリアル',
              to: '/docs/tutorial/intro',
            },
            {
              label: 'リファレンス',
              to: '/docs/reference/intro',
            },
          ],
        },
        {
          title: 'コミュニティ',
          items: [
            {
              label: 'Discord',
              href: 'https://discord.gg/CrsZKZQeWT',
            },
          ],
        },
        {
          title: 'その他',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/Se1getsu/shol',
            },
            {
              label: '制作者 X (旧 Twitter)',
              href: 'https://x.com/Se1getsu',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} 霽月.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
