import { defineConfig } from "astro/config";
import remarkLesetid from "remark-lesetid/astro";
import {
  rehypeTitles,
  rehypeCodeCopy,
  rehypePreClass,
  rehypeH1,
} from "./src/rehype";
import {
  rehypeHeadingIds,
  rehypeShiki,
  type ShikiConfig,
} from "@astrojs/markdown-remark";
import { remarkAlert } from "remark-github-blockquote-alert";
import react from "@astrojs/react";
import arraybuffer from "vite-plugin-arraybuffer";
import mdx from "@astrojs/mdx";
import { remarkMark } from "remark-mark-highlight";
import rehypeSectionHeadings from "@maxmmyron/rehype-section-headings";
import tailwindcss from "@tailwindcss/vite";

import { remarkCodeMeta } from "./src/remark";

// https://astro.build/config
export default defineConfig({
  // your configuration options here...
  // https://docs.astro.build/en/reference/configuration-reference/
  integrations: [
    mdx(),
    // tailwind({
    //   applyBaseStyles: false
    // }),
    react(),
  ],
  devToolbar: {
    enabled: false,
  },
  output: "static",
  site: "https://ayats.org",
  markdown: {
    gfm: true,
    syntaxHighlight: false,
    // shikiConfig: {
    //   // theme: 'github-dark',
    //     light: "github-light",
    //     dark: "ayu-dark",
    //   }
    // },
    remarkPlugins: [remarkMark, remarkAlert, remarkLesetid, remarkCodeMeta],
    rehypePlugins: [
      rehypeH1,
      rehypeHeadingIds,
      rehypeTitles,
      rehypeCodeCopy,
      [
        rehypeShiki,
        {
          themes: {
            dark: "github-dark",
            light: "github-light",
          },
        } as ShikiConfig,
      ],
      rehypePreClass,
      [
        rehypeSectionHeadings,
        {
          sectionDataAttribute: "data-heading-id",
        },
      ],
    ],
  },
  build: {
    format: "file",
  },
  vite: {
    plugins: [arraybuffer(), tailwindcss()],
    optimizeDeps: {
      exclude: ["@resvg/resvg-js"],
    },
  },
  trailingSlash: "never",

  // https://docs.astro.build/en/reference/experimental-flags/client-prerender
  prefetch: {
    prefetchAll: true,
    defaultStrategy: "viewport",
  },
  experimental: {
    clientPrerender: true,
  },
});
