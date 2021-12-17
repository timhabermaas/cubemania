/** Capitalizes the string by converting the first character to its uppercase variant.
 */
export function capitalize(name: string): string {
    return name.charAt(0).toUpperCase() + name.slice(1);
}
