import type { Root } from "hast";
import { u } from "unist-builder";
import { visit } from "unist-util-visit";
import type { Element } from "hast";

import { fromHtml } from 'hast-util-from-html'

import IconCopy from "@tabler/icons/outline/copy.svg?raw";
import IconCopyCheckFilled from "@tabler/icons/filled/copy-check.svg?raw";

function icon2node(raw: string): Element {
    return fromHtml(raw, {
        fragment: true,
    }).children[0] as Element;
}


export function rehypePreClass(): (tree: Root) => void {
    return function (tree: Root) {
        visit(tree, "element", node => {
            if (node.tagName === "pre") {
                if (node.properties.class === undefined) {
                    node.properties.class = "";
                }
                (node.properties.class as string) += " card";
            }
        });
    }
}

export function rehypeTitles(): (tree: Root) => void {
    return function (tree: Root) {
        visit(tree, 'element', function (node) {

            if (['h1', 'h2', 'h3', 'h4', 'h5'].includes(node.tagName)) {

                // must run after rehypeHeadingIds
                const id = node.properties["id"];

                if (typeof id === "string") {
                    const text = node.children[0];
                    if (text !== undefined) {
                        node.children = [
                            u("element", {
                                tagName: "a",
                                properties: {
                                    href: `#${id}`,
                                }
                            }, [
                                text
                            ]),
                        ];
                    }
                }
            }
        })
    }
}

export function rehypeCodeCopy(): (tree: Root) => void {
    return function (tree: Root) {
        visit(tree, 'element', function (node, index, parent) {
            if (node.tagName !== "pre") {
                return;
            }

            if (index !== undefined) {
                parent?.children.splice(index, 1, ...[
                    u("element", {
                        tagName: "div", properties: {
                            class: ["code-container"]
                        }
                    }, [
                        node,
                        u("element", {
                            tagName: "button",
                            properties: {
                                class: ["code-copy", "astro-button"],
                                "aria-label": "Copy code"
                            },
                        }, [
                            icon2node(IconCopy),
                            icon2node(IconCopyCheckFilled)
                        ])
                    ])
                ]);
            }
        })
    }
}
