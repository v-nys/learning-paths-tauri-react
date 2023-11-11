import { Lv1ReadResult } from './iointerfaces.ts';
import { Fragment } from 'react';

export interface ReadResultProps {
  value: Lv1ReadResult
}

export function ReadResult(props: ReadResultProps) {

  let err = props.value.Err;
  if (err) {
    return <p>{err}</p>
  }
  else {
    let ok = props.value.Ok;
    if (ok) {
      let comments = ok[0];
      let svg = ok[1];
      return (<>
        <div dangerouslySetInnerHTML={{ __html: svg }} />
        {comments.map((s) => <p key={s}>{s}</p>)}
      </>)
    }
    else {
      return <p>Programming error!</p>;
    }
  }
}
