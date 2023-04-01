import { useForm } from "react-hook-form";
import { useCreateComment } from "../hooks/useCreateComment";

interface CommentFormProps {
  postId: number;
  jwtToken?: string;
}

interface FormInput {
  content: string;
}

export function CommentForm(props: CommentFormProps) {
  const { register, handleSubmit, reset, formState, watch } =
    useForm<FormInput>({ defaultValues: { content: "" } });
  const createComment = useCreateComment(props.postId, props.jwtToken);

  const content = watch("content", "");

  function onSubmit(data: FormInput) {
    reset();
    createComment.mutate(data.content);
  }
  return (
    <form onSubmit={handleSubmit(onSubmit)}>
      <fieldset className="inputs">
        <ol>
          <li id="comment_content_input" className="text input required">
            <label className="label" htmlFor="comment_content">
              Content
            </label>
            <textarea
              id="comment_content"
              rows={4}
              {...register("content")}
            ></textarea>
          </li>
        </ol>
      </fieldset>
      <fieldset className="actions">
        <ol>
          <li id="comment_submit_action">
            <input
              disabled={!formState.isSubmitting && content.trim() === ""}
              type="submit"
              value={formState.isSubmitting ? "Responding" : "Respond"}
            />
          </li>
        </ol>
      </fieldset>
    </form>
  );
}
