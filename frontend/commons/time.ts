/** Formats a time given in ISO8601 format as a long day.
 *
 * e.g. "October 16, 2019"
 *
 * @param time the time given in ISO8601
 */
export function formatDate(time: string): string {
  return new Date(time).toLocaleDateString(undefined, {
    year: "numeric",
    month: "long",
    day: "numeric",
  });
}

/** Formats a given duration in ms to a duration using min and s. Doesn't display
 * the minutes if they are 0.
 *
 * 10000 => 10.00s
 * 60005 => 1:00.01s
 *
 * @param duration an integer containing the duration in ms
 * @returns a formatted duration, e.g. 2:03.52min
 */
export function formatDuration(duration: number): string {
  let seconds = Math.round(duration / 10) / 100;
  if (seconds < 60) {
    return `${seconds.toFixed(2)}s`;
  } else {
    const minutes = Math.floor(seconds / 60);
    seconds -= minutes * 60;
    const s = seconds < 10 ? "0" : "";
    return `${minutes}:${s}${seconds.toFixed(2)}min`;
  }
}