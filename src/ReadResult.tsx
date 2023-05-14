import { Lv1ReadResult, Lv2ReadResult } from './iointerfaces.ts';
import { Fragment } from 'react';

export interface ReadResultProps {
  value: Lv1ReadResult
}

export function ReadResult(props: ReadResultProps) {
  return props.value.Err ??
         props.value.Ok.Err ??
         <div dangerouslySetInnerHTML={{__html: props.value.Ok.Ok}} />
}
