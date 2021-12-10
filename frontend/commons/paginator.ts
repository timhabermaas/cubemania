const WINDOW_SIZE = 3;

/**
 * Returns the page numbers to display in the pagination component.
 *
 * @param current the currently selected page
 * @param total the total number of pages
 * @returns An array of consecutive page numbers with `null` representing gaps
 */
export function pages(current: number, total: number): (number[] | null)[] {
  if (total <= WINDOW_SIZE) {
    return [new Array(total).fill(null).map((_, i) => i + 1)];
  }

  let start = current - WINDOW_SIZE;
  let end = current + WINDOW_SIZE;

  if (start < 1) {
    start = 1;
  }

  if (end > total) {
    end = total;
  }

  let middleWindow = new Array(end - start + 1)
    .fill(null)
    .map((_, i) => i + start);

  let startWindow = [1, 2];
  let endWindow = [total - 1, total];

  let result: (number[] | null)[];

  if (startWindow[startWindow.length - 1] + 1 >= middleWindow[0]) {
    if (endWindow[0] - 1 <= middleWindow[middleWindow.length - 1]) {
      result = [makeUnique(startWindow.concat(middleWindow).concat(endWindow))];
    } else {
      result = [makeUnique(startWindow.concat(middleWindow)), null, endWindow];
    }
  } else {
    if (endWindow[0] - 1 <= middleWindow[middleWindow.length - 1]) {
      result = [startWindow, null, makeUnique(middleWindow.concat(endWindow))];
    } else {
      result = [startWindow, null, middleWindow, null, endWindow];
    }
  }

  return result;
}

function makeUnique<T>(list: T[]): T[] {
  return Array.from(new Set(list));
}
