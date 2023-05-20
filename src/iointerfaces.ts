export interface Lv2ReadResult {
  Ok?: string,
  Err?: string[]
}

export interface Lv1ReadResult {
  Ok?: Lv2ReadResult,
  Err?: string
}
