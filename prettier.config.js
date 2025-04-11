// @ts-check

/**
 * @type {import("prettier").Config}
 */
export default {
    tabWidth: 4,
    singleQuote: false,
    trailingComma: "all",
    semi: true,
    plugins: ["prettier-plugin-astro"],
    overrides: [
        {
            files: "*.astro",
            options: {
                parser: "astro",
            },
        },
    ],
};
