import { Lv1ReadResult, Lv2ReadResult } from './iointerfaces.ts';

export interface ReadResultProps {
  value: Lv1ReadResult
}

export function ReadResult(props: ReadResultProps) {
  return props.value.Err ??
         props.value.Ok.Err ??
         props.value.Ok.Ok
}
