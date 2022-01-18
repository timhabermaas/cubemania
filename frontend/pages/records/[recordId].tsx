import Link from "next/link";
import React from "react";
import { userPath } from "../../commons/path";
import { capitalize } from "../../commons/text";
import { formatDateAndTime, formatDuration } from "../../commons/time";
import { useRecord } from "../../hooks/useRecord";
import { useRecordIdFromUrl } from "../../hooks/useRecordIdFromUrl";
import { findRecordTypeByAmount } from "../../models/recordTypes";

export default function Record() {
  const recordId = parseInt(useRecordIdFromUrl(), 10);
  const { data } = useRecord(recordId);

  return (
    <>
      {data && (
        <>
          <h1>
            <Link href={userPath(data.user_slug)}>
              <a>{capitalize(data.user_name)}'s</a>
            </Link>{" "}
            {findRecordTypeByAmount(data.amount).name} Record
          </h1>

          <article id="record">
            <header>
              <h2>{formatDuration(data.time)}</h2>
              <small>{formatDateAndTime(data.set_at)}</small>
              <div className={`puzzle pos${data.puzzle_css_position}`}>
                <div className={`kind pos${data.kind_css_position}`}>
                  {data.puzzle_name} {data.kind_short_name}
                </div>
              </div>
            </header>

            <table id="singles">
              <thead>
                <tr>
                  <th>Solve</th>
                  <th>Time</th>
                  <th>Scramble</th>
                  <th>Comment</th>
                </tr>
              </thead>
              <tbody>
                {data.singles.map((single, i) => (
                  <tr key={single.id} className={(i + 1) % 2 ? "even" : "odd"}>
                    <td>{i + 1}</td>
                    <td>
                      <strong
                        className={`time ${single.penalty?.toLowerCase()}`}
                      >
                        {formatDuration(single.time)}
                      </strong>
                    </td>
                    <td>
                      <small>{single.scramble}</small>
                    </td>
                    <td>{single.comment}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </article>
        </>
      )}
    </>
  );
}
