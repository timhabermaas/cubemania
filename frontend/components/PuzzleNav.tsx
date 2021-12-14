import Link from "next/link";
import React, { useEffect, useRef, useState } from "react";
import { usePuzzleFromUrl } from "../hooks/usePuzzleFromUrl";
import { Kind, usePuzzles } from "../hooks/usePuzzles";

function movePuzzles(puzzleEl: HTMLUListElement, index: number) {
  puzzleEl.setAttribute("style", `left: ${-100 * index}%; width: 400%;`);
}

function findPuzzleIndex(kind: Kind[], puzzleSlug: string): number {
  const result = kind.findIndex((k) => {
    return k.puzzles.find((p) => p.slug === puzzleSlug) !== undefined;
  });

  return result < 0 ? 0 : result;
}

export function PuzzleNav() {
  const { data } = usePuzzles();
  const currentPuzzleSlug = usePuzzleFromUrl();

  const [currentKind, setCurrentKind] = useState<number>(
    findPuzzleIndex(data ?? [], currentPuzzleSlug)
  );

  const puzzlesRef = useRef<HTMLUListElement>(null);

  useEffect(() => {
    if (!data) {
      return;
    }

    let kind = findPuzzleIndex(data, currentPuzzleSlug);
    setCurrentKind(kind);

    if (puzzlesRef.current) {
      movePuzzles(puzzlesRef.current, kind);
    }
  }, [data, currentPuzzleSlug]);

  const onKindClick = (event: React.MouseEvent, i: number) => {
    event.preventDefault();

    if (puzzlesRef.current) {
      movePuzzles(puzzlesRef.current, i);
      setCurrentKind(i);
    }
  };

  if (data === undefined) {
    return null;
  }

  return (
    <nav id="subnavigation">
      <div id="puzzles">
        <ul ref={puzzlesRef} style={{ width: "400%", left: "0%" }}>
          {data.map((k) => (
            <li key={k.id} style={{ width: `${100 / data.length}%` }}>
              <ul className="puzzles">
                {k.puzzles.map((p) => (
                  <li
                    key={p.id}
                    className={p.slug === currentPuzzleSlug ? "checked" : ""}
                  >
                    <Link href={`/puzzles/${p.slug}/records`}>
                      <a>
                        <span className={`puzzle pos${p.css_position}`}>
                          <span className={`kind pos${k.css_position}`}></span>
                        </span>
                        <span className="name">{p.name}</span>
                      </a>
                    </Link>{" "}
                  </li>
                ))}
              </ul>
            </li>
          ))}
        </ul>
      </div>
      <div id="kinds">
        <ul className="center">
          {data.map((kind, i) => (
            <li
              key={kind.id}
              style={{ width: `${100 / data.length}%` }}
              className={i == currentKind ? "checked" : ""}
            >
              <a
                href="#"
                onClick={(e) => {
                  onKindClick(e, i);
                }}
              >
                {kind.name}
              </a>
            </li>
          ))}
        </ul>
      </div>
    </nav>
  );
}
