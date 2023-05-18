import { useState, useEffect, useRef, Fragment } from "react";
import { ReadResult } from "./ReadResult.tsx";
import { Lv1ReadResult, Lv2ReadResult } from "./iointerfaces.ts";
import reactLogo from "./assets/react.svg";
import { invoke } from "@tauri-apps/api/tauri";
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

function App() {

  const [loading, setLoading] = useState(false);
  const [paths, setPaths] = useState("");
  const [readResults, setReadResults] = useState(new Map());
  const [pathToDisplayOnceRead, setPathToDisplayOnceRead] = useState(undefined);
  const stopCallbacks = useRef([]);
  const [eventToHandle, setEventToHandle] = useState<undefined|"filechange"|"pathchange">(undefined);

  function stopWatching() {
    for (let callback of stopCallbacks.current) {
      callback();
    }
    stopCallbacks.current = [];
  }

  async function waitForEvents(parent,children) {
      return await watch(
          parent,
          // actually produces an array, not a single event!
          (events) => {
              console.log("event noticed");
              if (!eventToHandle) {
                let shouldReload = false;
                for (let {path} of events) {
                    if (children.includes(path)) {
                        shouldReload = true;
                    }
                }
                if (shouldReload) {
                  setEventToHandle("filechange");
                  console.log("file change!");
                }
              }
              else {
                console.log(`still have to deal with ${eventToHandle}`);
              }
          },
          { recursive: false }
      );
  }

  async function startWatching() {
    const separatePaths = separateIntoUniquePaths(paths);
    const newStopcallbacks = [];
    try {
        const association = await invoke('associate', { paths: separatePaths.join(";") });
        // this is an object, not a map!
        for (const parent in association) {
          const children = association[parent];
          newStopcallbacks.push(await waitForEvents(parent,children));
        }
    }
    catch (err) {
        console.log(`${err} lacks a watchable parent node`);
    }
    stopCallbacks.current = newStopcallbacks;
  }

  async function readFileContents() {
      let separatePaths = separateIntoUniquePaths(paths);
      let svgs = await invoke('read_contents', { paths: separatePaths.join(";") });
      let newReadResults = new Map<string,Lv1ReadResult>();
      svgs.forEach((pair) => { newReadResults.set(pair[0], pair[1]); });
      setReadResults(newReadResults);
  }

  useEffect(() => {
    setEventToHandle("pathchange");
  }, [paths]);

  useEffect(() => {
      const separatePaths = separateIntoUniquePaths(paths);
      if (!separatePaths.length) {
        setPathToDisplayOnceRead(undefined);
      }
      else if (!separatePaths.includes(pathToDisplayOnceRead)) {
        setPathToDisplayOnceRead(separatePaths[0]);
      }
  }, [paths]);

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
              setReadResults(new Map());
          }
          setLoading(false);
          break;
        }
        case "filechange": {
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
  })(), [loading,eventToHandle]);

  const onOptionChange = (e) => {
    setPathToDisplayOnceRead(e.target.value);
  }

  return (
    <>
    <p>{eventToHandle} {loading ? "loading" : "not loading"}</p>
    <div className="container">
      <div className="row">
        <input
          id="files-input"
          onChange={(e) => setPaths(e.currentTarget.value)}
          placeholder="Enter &quot;;&quot;-separated paths"
        />
      </div>
      {
        loading ?
        <p>Please hold</p> :
        <div className="row">
            {
             Array.from(readResults.keys())
                   .map((k) => {
                return (<Fragment key={k}>
                          <input
                            name="active-path"
                            type="radio"
                            id={`radio-button-${k}`}
                            value={k}
                            checked={pathToDisplayOnceRead === k}
                            onChange={onOptionChange}
                            />
                          <label htmlFor={`radio-button-${k}`}>{k}</label>
                        </Fragment>
                       )
              })
            }
        </div>
      }
      {
        readResults.get(pathToDisplayOnceRead) ?
        (loading ?
         // possible when watched file changes
         <p>Loading</p> :
         <ReadResult value={readResults.get(pathToDisplayOnceRead)} />) :
        <p>Cannot display anything with this input.</p>
      }
    </div>
    </>
  );
}

export default App;
