import React from "react";
import Link from "next/link";
import {
  postPath,
  records3x3x3Path,
  timer3x3x3Path,
  userPath,
} from "../commons/path";
import { useAnnouncement } from "../hooks/useAnnouncement";
import ReactMarkdown from "react-markdown";

interface AnnouncementProps {
  url: string;
  title: string;
  content: string;
  commentsCount: number;
}

function Announcement(props: AnnouncementProps) {
  return (
    <article className="announcement">
      <strong>{props.title}</strong>{" "}
      <ReactMarkdown unwrapDisallowed={true} disallowedElements={["p"]}>
        {props.content}
      </ReactMarkdown>{" "}
      <Link href={props.url}>
        <a>{props.commentsCount} Comments »</a>
      </Link>
    </article>
  );
}

export default function Home() {
  const { data } = useAnnouncement();

  return (
    <>
      {data && (
        <Announcement
          title={data.title}
          content={data.content}
          commentsCount={data.comments_count}
          url={postPath(data.id)}
        />
      )}
      <p className="introduction">
        You want to keep track of your times, compare yourself with others and
        become the best? If so, Cubemania is the right place for you:{" "}
        <a href="/register">Register</a> now and get the record!
      </p>
      <ul id="features">
        <li className="odd">
          <a href={timer3x3x3Path} className="image">
            <img alt="Timer" src="/screenshots/timer.jpg" />
          </a>
          <br />
          <p>Stop your times and submit your averages.</p>
        </li>
        <li>
          <a href={userPath("tim")} className="image">
            <img alt="Puzzles" src="/screenshots/puzzles.jpg" />
          </a>
          <br />
          <p>Organize your times properly.</p>
        </li>
        <li className="odd">
          <a href={timer3x3x3Path} className="image">
            <img alt="Chart" src="/screenshots/chart.jpg" />
          </a>
          <p>
            Keep track of your progress and compare yourself with other cubers.
          </p>
        </li>
        <li>
          <a href={records3x3x3Path} className="image">
            <img alt="Records" src="/screenshots/records.jpg" />
          </a>
          <p>Get the record!</p>
        </li>
      </ul>
    </>
  );
}
