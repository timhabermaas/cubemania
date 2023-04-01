import React from "react";
import { userPath } from "../../commons/path";
import { usePostIdFromUrl } from "../../hooks/usePostIdFromUrl";
import markdown from "markdown-it";
import { usePost } from "../../hooks/usePost";
import { formatDate, formatDateAndTime } from "../../commons/time";
import Link from "next/link";
import { useMyRole } from "../../hooks/useMyRole";
import { useDeleteComment } from "../../hooks/useDeleteComment";
import { useMe } from "../../hooks/useMe";
import { CommentForm } from "../../components/CommentForm";

interface RecordProps {
  jwtToken?: string;
}

export default function Post(props: RecordProps) {
  const { data: meData } = useMe(props.jwtToken);
  const postId = parseInt(usePostIdFromUrl(), 10);
  const { data } = usePost(postId);
  const userRole = useMyRole(props.jwtToken);
  const deleteComment = useDeleteComment(postId, props.jwtToken);
  const currentUser = meData && meData.current_user;

  let md = markdown();

  if (data === undefined) {
    return null;
  }

  const onDeleteCommentClick = (commentId: number) => {
    if (confirm(`Do you really want to remove this comment?`)) {
      deleteComment.mutate(commentId);
    }
  };

  return (
    <div className="post" id={`post_${postId}`}>
      <article className="post">
        <h1>{data.title}</h1>
        <section
          className="text"
          dangerouslySetInnerHTML={{ __html: md.render(data.content) }}
        ></section>
        <div className="meta">
          {formatDate(data.created_at)} ·{" "}
          <cite>
            <Link href={userPath(data.user_slug)}>
              <a>{data.user_name}</a>
            </Link>
          </cite>
        </div>
      </article>
      <div id="comments" className="comments">
        {data.comments.length > 0 && (
          <ol className="comments">
            {data.comments.map((c) => (
              <li key={c.id} className="comment">
                <cite>
                  <Link href={userPath(c.user_slug)}>
                    <a>{c.user_name}</a>
                  </Link>
                </cite>
                <small>
                  {formatDateAndTime(c.created_at)}{" "}
                  {currentUser &&
                    (currentUser.slug === c.user_slug ||
                      userRole === "admin") && (
                      <a
                        href="#"
                        onClick={(event) => {
                          event.preventDefault();
                          onDeleteCommentClick(c.id);
                        }}
                      >
                        Delete
                      </a>
                    )}
                </small>
                <div
                  className="text"
                  dangerouslySetInnerHTML={{ __html: md.render(c.content) }}
                ></div>
              </li>
            ))}
          </ol>
        )}
        {currentUser && (
          <CommentForm jwtToken={props.jwtToken} postId={postId} />
        )}
      </div>
    </div>
  );
}
