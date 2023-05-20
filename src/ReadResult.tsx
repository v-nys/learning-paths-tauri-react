import { Lv1ReadResult, Lv2ReadResult } from './iointerfaces.ts';
import { Fragment } from 'react';

export interface ReadResultProps {
  value: Lv1ReadResult
}

export function ReadResult(props: ReadResultProps) {
  const errs = props.value.Ok.Err;
  return props.value.Err ??
         (errs ? Array.from(new Set(errs)).map((e) => <p key={e}>{e}</p>) : undefined) ??
         <div dangerouslySetInnerHTML={{__html: props.value.Ok.Ok}} />
}
