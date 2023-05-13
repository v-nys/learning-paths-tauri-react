import { useState, useEffect } from "react";
import reactLogo from "./assets/react.svg";
import { invoke } from "@tauri-apps/api/tauri";
import "./App.css";

interface Lv2ReadResult {
  Ok?: string,
  Err?: string
}

interface Lv1ReadResult {
  Ok?: Lv2ReadResult,
  Err?: string
}

interface Association {
  Ok?: object,
  Err?: string
}

function separateIntoUniquePaths(paths) {
  let separatePaths = paths.split(";").map((p) => p.trim()).filter((p) => p !== "");
  return [...new Set(separatePaths)];
}

function App() {

  const [paths, setPaths] = useState("");
  const [readResults, setReadResults] = useState(new Map());

  console.log(readResults);

  useEffect(() => {
      stopWatching();
      void (async () => {
          if (paths.trim()) {
              await readFileContents();
              await startWatching();
          }
      })();
  }, [paths]);

  async function readFileContents() {
      // example paths: /home/vincent/Projects/tauritest/src-tauri/test/git.yaml;/home/vincent/Projects/tauritest/src-tauri/test/got.yaml
      let separatePaths = separateIntoUniquePaths(paths);
      let svgs = await invoke('read_contents', { paths: separatePaths.join(";") });
      let newReadResults = new Map<string,Lv1ReadResult>();
      svgs.forEach((pair) => { newReadResults.set(pair[0], pair[1]); });
      setReadResults(newReadResults);
  }

  function stopWatching() {
    console.log("TODO: stop watching currently watched files.");
  }

  async function startWatching() {
    let separatePaths = separateIntoUniquePaths(paths);
    try {
        const association = await invoke('associate', { paths: separatePaths.join(";") });
        console.log(association);
    }
    catch (err) {
        console.log(`${err} lacks a watchable parent node`);
    }
  }

  return (
    <div className="container">
      <h1>Welcome to Tauri!</h1>

      <div className="row">
        <a href="https://vitejs.dev" target="_blank">
          <img src="/vite.svg" className="logo vite" alt="Vite logo" />
        </a>
        <a href="https://tauri.app" target="_blank">
          <img src="/tauri.svg" className="logo tauri" alt="Tauri logo" />
        </a>
        <a href="https://reactjs.org" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>

      <div className="row">
        <input
          id="files-input"
          onChange={(e) => setPaths(e.currentTarget.value)}
          placeholder="Enter &quot;;&quot;-separated paths"
        />
      </div>
      <p>Need a dropdown or something to select graph here.</p>
    </div>
  );
}

export default App;
