import { assertNotUndefined } from "../commons/types/assertions";

export const RecordTypes = [
  { name: "Single", short: "single", amount: 1 },
  { name: "Average of 5", short: "avg5", amount: 5 },
  { name: "Average of 12", short: "avg12", amount: 12 },
] as const;

export function findRecordTypeByAmount(
  amount: number
): typeof RecordTypes[number] {
  return assertNotUndefined(RecordTypes.find((r) => r.amount === amount));
}
