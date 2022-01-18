import Link from "next/link";
import React from "react";
import { pages } from "../../../../commons/paginator";
import { recordsPath, userPath } from "../../../../commons/path";
import { formatDate, formatDuration } from "../../../../commons/time";
import { PaginatedResponse } from "../../../../commons/types/PaginatedResponse";
import { useBlockUser } from "../../../../hooks/useBlockUser";
import { useMyRole } from "../../../../hooks/useMyRole";
import { usePuzzleFromUrl } from "../../../../hooks/usePuzzleFromUrl";
import { useRecords, Record } from "../../../../hooks/useRecords";
import { useRecordTypeFromUrl } from "../../../../hooks/useRecordTypeFromUrl";
import { useSingleQueryParam } from "../../../../hooks/useSingleQueryParam";
import { RecordTypes } from "../../../../models/recordTypes";

interface RecordsProps {
  jwtToken?: string;
}

function RecordTypeTabs() {
  const currentPuzzleSlug = usePuzzleFromUrl();
  const currentType = useRecordTypeFromUrl();

  return (
    <p className="tabs">
      {RecordTypes.map((t) => (
        <React.Fragment key={t.short}>
          <Link href={recordsPath(currentPuzzleSlug, undefined, t.short)}>
            <a className={t.short === currentType ? "selected" : ""}>
              {t.name}
            </a>
          </Link>{" "}
        </React.Fragment>
      ))}
    </p>
  );
}

interface PaginationProps {
  records: PaginatedResponse<Record>;
  puzzleSlug: string;
  type: string;
}

function Pagination(props: PaginationProps) {
  let pageNumbers = pages(
    props.records.page,
    Math.ceil(props.records.total_item_count / props.records.max_items_per_page)
  );

  const previous_page = (
    <Link
      href={recordsPath(props.puzzleSlug, props.records.page - 1, props.type)}
    >
      <a className="previous_page" rel="prev start">
        ← Previous
      </a>
    </Link>
  );

  const next_page = (
    <Link
      href={recordsPath(props.puzzleSlug, props.records.page + 1, props.type)}
    >
      <a rel="next">Next →</a>
    </Link>
  );

  const numbers = () => {
    return (
      <>
        {pageNumbers.flatMap((chunk, i) => {
          if (chunk === null) {
            return [
              // NOTE: index as key is fine since all gaps are interchangeable.
              <span key={`gap-${i}`} className="gap">
                …
              </span>,
              " ",
            ];
          } else {
            return chunk.map((n) => {
              if (n === props.records.page) {
                return (
                  <React.Fragment key={`number-${n}`}>
                    <em className="current">{n}</em>{" "}
                  </React.Fragment>
                );
              }
              return (
                <React.Fragment key={`number-${n}`}>
                  <Link href={recordsPath(props.puzzleSlug, n, props.type)}>
                    <a rel="prev start">{n}</a>
                  </Link>{" "}
                </React.Fragment>
              );
            });
          }
        })}
      </>
    );
  };

  return (
    <div className="pagination">
      <div className="pagination">
        {props.records.page > 1 && previous_page}{" "}
        {props.records.page === 1 && (
          <>
            <span className="disabled">← Previous</span>{" "}
          </>
        )}
        {numbers()}{" "}
        {props.records.next_page === null && (
          <span className="disabled">Next →</span>
        )}
        {props.records.next_page !== null && next_page}
      </div>
    </div>
  );
}

interface RecordTableProps {
  records: PaginatedResponse<Record>;
  puzzleSlug: string;
  type: string;
  jwtToken?: string;
}

function RecordTable(props: RecordTableProps) {
  const userRole = useMyRole(props.jwtToken);
  const blockMutation = useBlockUser(props.jwtToken);

  const blockUser = (event: React.MouseEvent, slug: string, name: string) => {
    event.preventDefault();

    if (confirm(`Do you really want to block ${name}?`)) {
      blockMutation.mutate(slug);
    }
  };

  return (
    <table id="records">
      <tbody>
        {props.records.items.map((r) => (
          <tr key={r.id} className={`record rank${r.rank}`}>
            <th>
              <span>{r.rank}</span>
            </th>
            <td>
              <strong>{formatDuration(r.time)}</strong>
            </td>
            <td>
              <cite>
                <a href={userPath(r.user_slug)}>{r.user_name}</a>
              </cite>
            </td>
            {userRole === "admin" && (
              <td>
                <a
                  rel="nofollow"
                  href="#"
                  onClick={(event) =>
                    blockUser(event, r.user_slug, r.user_name)
                  }
                >
                  Block!
                </a>
              </td>
            )}
            <td>
              <small>{formatDate(r.set_at)}</small>
            </td>
            <td>
              <blockquote>{r.comment}</blockquote>
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

export default function Records(props: RecordsProps) {
  const currentPuzzleSlug = usePuzzleFromUrl();
  const currentType = useRecordTypeFromUrl();
  const page = parseInt(useSingleQueryParam("page") ?? "1", 10);
  const { data } = useRecords(currentType, page, currentPuzzleSlug);

  return (
    <>
      <RecordTypeTabs />
      {data && (
        <RecordTable
          records={data.records}
          puzzleSlug={currentPuzzleSlug}
          type={currentType}
          jwtToken={props.jwtToken}
        />
      )}
      {data && (
        <Pagination
          type={currentType}
          puzzleSlug={currentPuzzleSlug}
          records={data.records}
        />
      )}
    </>
  );
}
