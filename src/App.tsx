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

  // TODO: also pause / restrict reacting to changes while loading
  // otherwise auto-saving editors will be an issue
  const [loading, setLoading] = useState(false);
  const [paths, setPaths] = useState("");
  const [readResults, setReadResults] = useState(new Map());
  const [pathToDisplayOnceRead, setPathToDisplayOnceRead] = useState(undefined);
  const stopCallbacks = useRef([]);

  useEffect(() => {
      const separatePaths = separateIntoUniquePaths(paths);
      if (!separatePaths.length) {
        setPathToDisplayOnceRead(undefined);
      }
      else if (!separatePaths.includes(pathToDisplayOnceRead)) {
        setPathToDisplayOnceRead(separatePaths[0]);
      }
  }, [paths]);

  useEffect(() => {
      stopWatching();
      void (async () => {
          setLoading(true);
          if (paths.trim()) {
              await readFileContents();
              await startWatching();
          }
          else {
              setReadResults(new Map());
          }
          setLoading(false);
      })();
  }, [paths]);

  async function waitForEvents(parent,children) {
      return await watch(
          parent,
          // actually produces an array, not a single event!
          (events) => {
              let shouldReload = false;
              for (let {path} of events) {
                  if (children.includes(path)) {
                      shouldReload = true;
                  }
              }
              if (shouldReload) {
                setLoading(true);
                readFileContents();
                setLoading(false);
              }
          },
          { recursive: false }
      );
  }

  async function readFileContents() {
      // example paths: /home/vincent/Projects/tauritest/src-tauri/test/git.yaml;/home/vincent/Projects/tauritest/src-tauri/test/got.yaml
      let separatePaths = separateIntoUniquePaths(paths);
      let svgs = await invoke('read_contents', { paths: separatePaths.join(";") });
      let newReadResults = new Map<string,Lv1ReadResult>();
      svgs.forEach((pair) => { newReadResults.set(pair[0], pair[1]); });
      setReadResults(newReadResults);
  }

  function stopWatching() {
    for (let callback of stopCallbacks.current) {
      callback();
    }
    stopCallbacks.current = [];
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

  const onOptionChange = (e) => {
    setPathToDisplayOnceRead(e.target.value);
  }

  return (
    <div className="container">
      <div className="row">
        <input
          id="files-input"
          onChange={(e) => setPaths(e.currentTarget.value)}
          placeholder="Enter &quot;;&quot;-separated paths"
          disabled={loading}
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
        <ReadResult value={readResults.get(pathToDisplayOnceRead)} /> :
        <p>Cannot display anything with this input.</p>
      }
    </div>
  );
}

export default App;
