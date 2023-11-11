import { Lv1ReadResult } from './iointerfaces.ts';
import { Fragment } from 'react';

export interface ReadResultProps {
  value: Lv1ReadResult
}

export function ReadResult(props: ReadResultProps) {
  /*const errs = props.value.Ok?.Err;
  return props.value.Err ??
         (errs ? Array.from(new Set(errs)).map((e) => <p key={e}>{e}</p>) : undefined) ??
         <>
           {props.value.Ok.Ok[1].map((s) => <p key={s}>{s}</p>)}
           <div dangerouslySetInnerHTML={{__html: props.value.Ok.Ok[0]}} />
         </>*/
         // return props.value.Err ?? <p>Looking good.</p>
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
              {comments.map((s) => <p key={s}>{s}</p>)}
              <div dangerouslySetInnerHTML={{__html: svg}} />
            </>)
          }
          else {
            return <p>Programming error!</p>;
          }
         }
}
