import { useState, useEffect, useRef, Fragment } from "react";
import TextareaAutosize from 'react-textarea-autosize';
import { ReadResult } from "./ReadResult.tsx";
import { Lv1ReadResult } from "./iointerfaces.ts";
import { invoke } from "@tauri-apps/api/tauri";
import { appWindow } from "@tauri-apps/api/window";
import { watch } from "tauri-plugin-fs-watch-api";
import "./App.css";

interface Association {
  Ok?: object,
  Err?: string
}

function separateIntoUniquePaths(paths) {
  let separatePaths = paths.split(";").map((p) => p.trim()).filter((p) => p !== "");
  return [...new Set(separatePaths)];
}

// this is passed off to watch, so it shouldn't capture any component state
// that would get outdated
// instead, use signals to communicate
async function waitForEvents(parent, children: any[]) {
  console.log(`Registering watch for ${parent}`);
  return await watch(
    parent,
    // actually produces an array, not a single event
    (events) => {
      console.debug(events);
      let shouldReload = false;
      for (let { path } of events) {
        if (children.find((child) => path.startsWith(child))) {
          shouldReload = true;
        }
      }
      if (shouldReload) {
        appWindow.emit('filechange');
      }
    },
    { recursive: true }
  );
}

function findCommonPrefix(words: string[]): string | undefined {
  // check border cases size 1 array and empty first word)
  if (!words[0] || words.length == 1) return words[0] || "";
  let i = 0;
  // while all words have the same character at position i, increment i
  while (words[0][i] && words.every(w => w[i] === words[0][i]))
    i++;
  // prefix is the substring from the beginning to the last successfully checked i
  return words[0].substr(0, i);
}

const COMPLETE_GRAPH_LABEL = "Supercluster";

function App() {

  // TODO: consider making more use of useReducer
  const [loading, setLoading] = useState(false);
  const [zipping, setZipping] = useState(false);
  const [paths, setPaths] = useState("");
  const [learningPath, setLearningPath] = useState("");
  const [learningPathComments, setLearningPathComments] = useState<string[]>([]);
  const [readResults, setReadResults] = useState(new Map<string, Lv1ReadResult>());
  const [pathToDisplayOnceRead, setPathToDisplayOnceRead] = useState(undefined);
  const stopCallbacks = useRef([]);
  // file and settings change really call for the same actions and have same level of precedence, so...
  const [eventToHandle, setEventToHandle] = useState<undefined | "fileorsettingschange" | "pathchange">(undefined);
  /* Setting the type of event to handle is different depending on situation.
   * A path change just occurs when the input field is modified.
   * A file change is signaled from outside the component code.
   */
  useEffect(() => {
    const onFileChange = () => {
      if (!eventToHandle) {
        setEventToHandle("fileorsettingschange");
      }
    }
    const unlisten = appWindow.listen('filechange', onFileChange);
    return async () => { const fn = await unlisten; fn(); }
  });

  useEffect(() => {
    if (!eventToHandle) {
      setEventToHandle("fileorsettingschange");
    }
  }, []);

  useEffect(() => {
    setEventToHandle("pathchange");
  }, [paths]);

  function stopWatching() {
    for (let callback of stopCallbacks.current) {
      callback();
    }
    stopCallbacks.current = [];
  }

  async function startWatching() {
    const separatePaths = separateIntoUniquePaths(paths);
    console.debug("SEPARATED");
    const newStopcallbacks = [];
    try {
      const association = await invoke('associate_parents_children', { paths: separatePaths.join(";") });
      console.debug(association);
      // this is an object, not a map!
      for (const parent in association) {
        const children = association[parent];
        newStopcallbacks.push(await waitForEvents(parent, children));
      }
    }
    catch (err) {
      console.log(`${err} lacks a watchable parent node`);
    }
    stopCallbacks.current = newStopcallbacks;
  }

  async function readFileContents() {
    let separatePaths = separateIntoUniquePaths(paths);
    let svgs = await invoke('read_contents',
      { paths: separatePaths.join(";") });
    let newReadResults = new Map<string, Lv1ReadResult>();
    svgs.forEach((pair) => { newReadResults.set(pair[0], pair[1]); });
    setReadResults(newReadResults);
  }

  async function checkLearningPath() {
    console.debug(learningPath.split(new RegExp("\\s+")).filter((e) => e !== ""));
    let resultOfCheck = await invoke('check_learning_path_stateful', {
      nodes: learningPath.split(new RegExp("\\s+")).filter((e) => e !== "")
    });
    setLearningPathComments(resultOfCheck);
  }

  useEffect(() => {
    if (learningPath.trim() !== "") {
      checkLearningPath();
    }
    else {
      setLearningPathComments([]);
    }
  }, [learningPath]);

  useEffect(() => {
    const separatePaths = separateIntoUniquePaths(paths);
    if (!separatePaths.length) {
      setPathToDisplayOnceRead(undefined);
    }
    else if (!separatePaths.includes(pathToDisplayOnceRead)) {
      setPathToDisplayOnceRead(separatePaths[0]);
    }
  }, [paths]);

  let filesRead = Array.from(readResults.keys());
  let commonPrefix = findCommonPrefix(filesRead.filter((path) => path !== COMPLETE_GRAPH_LABEL));

  useEffect(() => void (async () => {
    if (!loading) {
      const toHandle = eventToHandle;
      setEventToHandle(undefined);
      switch (toHandle) {
        case "pathchange": {
          setLoading(true);
          stopWatching();
          if (paths.trim()) {
            await readFileContents();
            await startWatching();
          }
          else {
            setReadResults(new Map<string, Lv1ReadResult>());
          }
          setLoading(false);
          break;
        }
        case "fileorsettingschange": {
          setLoading(true);
          await readFileContents();
          setLoading(false);
          break;
        }
        default: {
          break;
        }
      }
    }
  })(), [loading, eventToHandle]);

  const onOptionChange = (e) => {
    setPathToDisplayOnceRead(e.target.value);
  }

  return (
    <>
      <div className="container">
        <TextareaAutosize
          id="files-input"
          onChange={(e) => setPaths(e.currentTarget.value)}
          placeholder="Enter &quot;;&quot;-separated paths"
        />

        {
          loading ?
            <p>Please hold</p> :
            <div className="row">
              {
                /* Could split into separate component?
                 * long paths are irritating, by the way
                 * could compute common prefix for everything but complete graph
                 * would do this by folding
                 */

                Array.from(readResults.entries())
                  .map(([k, v]) => {
                    let icon = "✅";
                    if (v.Err) {
                      icon = "✖️"
                    }
                    else if (v.Ok[0].length > 0) {
                      icon = "⚠️"
                    }
                    return (<div key={k}>
                      <input
                        name="active-path"
                        type="radio"
                        id={`radio-button-${k}`}
                        value={k}
                        checked={pathToDisplayOnceRead === k}
                        onChange={onOptionChange}
                      />
                      <label htmlFor={`radio-button-${k}`}>{commonPrefix !== k && k !== COMPLETE_GRAPH_LABEL ? `...${k.substring(commonPrefix.length)}` : k} {icon}</label>
                    </div>
                    )
                  })
              }
            </div>

              
            
        }
        {
          readResults.get(pathToDisplayOnceRead) ?
            (loading ?
              <p>Loading</p> :
              <ReadResult value={readResults.get(pathToDisplayOnceRead)} />) :
            <p>Cannot display anything with this input.</p>
        }
        {
          loading || pathToDisplayOnceRead !== COMPLETE_GRAPH_LABEL ?
          <></> :
          <><TextareaAutosize
            value={learningPath}
            placeholder="enter whitespace-separated nodes that make up a learning path"
            onChange={async (e) => { setLearningPath(e.target.value) }} />
            { learningPathComments.map((comment, idx) => <p key={idx}>{comment}</p>) }
          </>
        }
        {
          loading || pathToDisplayOnceRead !== COMPLETE_GRAPH_LABEL || learningPathComments.length > 0 || Array.from(readResults.values()).some((readResult) => readResult.Ok && readResult.Ok[0].length > 0) ?
          <></> :
          <button disabled={zipping} onClick={ async (e) => { setZipping(true) ; let zipResult = await invoke('build_zip', { paths }) ; console.debug(zipResult) ; setZipping(false); }}>Zip it!</button>
        }
      </div>
    </>
  );
}

export default App;
