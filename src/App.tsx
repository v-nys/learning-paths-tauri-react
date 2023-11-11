import { useState, useEffect, useRef, Fragment } from "react";
import { ReadResult } from "./ReadResult.tsx";
import { Lv1ReadResult, Lv2ReadResult } from "./iointerfaces.ts";
import reactLogo from "./assets/react.svg";
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
async function waitForEvents(parent,children) {
  return await watch(
      parent,
      // actually produces an array, not a single event
      (events) => {
          let shouldReload = false;
          for (let {path} of events) {
              if (children.includes(path)) {
                  shouldReload = true;
              }
          }
          if (shouldReload) {
            appWindow.emit('filechange');
          }
      },
      { recursive: false }
  );
}



function App() {

  // TODO: consider making more use of useReducer
  const [loading, setLoading] = useState(false);
  const [paths, setPaths] = useState("");
  const [readResults, setReadResults] = useState(new Map());
  const [pathToDisplayOnceRead, setPathToDisplayOnceRead] = useState(undefined);
  const stopCallbacks = useRef([]);
  // file and settings change really call for the same actions and have same level of precedence, so...
  const [eventToHandle, setEventToHandle] = useState<undefined|"fileorsettingschange"|"pathchange">(undefined);
  const [checkRedundantEdges, setCheckRedundantEdges] = useState(true);
  const [checkClusterBoundaries, setCheckClusterBoundaries] = useState(true);
  const [checkMissingFiles, setCheckMissingFiles] = useState(true);
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
  }, [checkRedundantEdges,checkClusterBoundaries,checkMissingFiles]);

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
      let svgs = await invoke('read_contents',
      { paths: separatePaths.join(";"),
        checkRedundantEdges: checkRedundantEdges,
        checkClusterBoundaries: checkClusterBoundaries,
        checkMissingFiles: checkMissingFiles });
      let newReadResults = new Map<string,Lv1ReadResult>();
      svgs.forEach((pair) => { newReadResults.set(pair[0], pair[1]); });
      setReadResults(newReadResults);
  }

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
  })(), [loading,eventToHandle]);

  const onOptionChange = (e) => {
    setPathToDisplayOnceRead(e.target.value);
  }

  return (
    <>
    <div className="container">
      <div className="row">
        <input
	      type="checkbox"
          id="redundant-edges-input"
	      checked={checkRedundantEdges}
          onChange={() => setCheckRedundantEdges(!checkRedundantEdges)}
        />
	    <label htmlFor="redundant-edges-input">check for redundant edges</label>
      </div>

      <div className="row">
        <input
	      type="checkbox"
          id="cluster-boundaries-input"
	      checked={checkClusterBoundaries}
          onChange={() => setCheckClusterBoundaries(!checkClusterBoundaries)}
        />
	    <label htmlFor="cluster-boundaries-input">check cluster boundaries</label>
	  </div>

      <div className="row">
        <input
	      type="checkbox"
          id="check-missing-files-input"
	      checked={checkMissingFiles}
          onChange={() => setCheckMissingFiles(!checkMissingFiles)}
        />
	    <label htmlFor="check-missing-files-input">check for missing files</label>
	  </div>



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
        <><div className="row">
            {
            /* Could split into separate component?
            */
             Array.from(readResults.entries())
                   .map(([k,v]) => {
                let icon = "✅";
                console.debug([k,v]);
                if (v.Err) {
                  icon = "✖️"
                }
                else if (v.Ok[0].length > 0) {
                  icon = "⚠️"
                }
                return (<Fragment key={k}>
                          <input
                            name="active-path"
                            type="radio"
                            id={`radio-button-${k}`}
                            value={k}
                            checked={pathToDisplayOnceRead === k}
                            onChange={onOptionChange}
                            />
                          <label htmlFor={`radio-button-${k}`}>{k} {icon}</label>
                        </Fragment>
                       )
              })
            } 
        </div>
        {
          // textarea positioning is unfortunate
          // but can fix this later on
        }
        
        <textarea placeholder="enter space-separated nodes that make up a learning path">

        </textarea>
        </>
      }
      {
        readResults.get(pathToDisplayOnceRead) ?
        (loading ?
         <p>Loading</p> :
         <ReadResult value={readResults.get(pathToDisplayOnceRead)} />) :
        <p>Cannot display anything with this input.</p>
      }
    </div>
    </>
  );
}

export default App;
