import { useState, useEffect, useRef } from "react";
import TextareaAutosize from 'react-textarea-autosize';
import { ReadResult } from "./ReadResult";
import { Lv1ReadResult } from "./iointerfaces";
import { invoke } from "@tauri-apps/api/tauri";
import { appWindow } from "@tauri-apps/api/window";
import { watch, DebouncedEvent } from "tauri-plugin-fs-watch-api";
import "./App.css";

function separateIntoUniquePaths(paths: string) {
    let separatePaths = paths.split(";").map((p) => p.trim()).filter((p) => p !== "");
    return [...new Set(separatePaths)];
}

// this is passed off to watch, so it shouldn't capture any component state
// that would get outdated
// instead, use signals to communicate
async function waitForEvents(parent: string, children: any[]) {
    console.log(`Registering watch for ${parent}`);
    return await watch(
        parent,
        // actually produces an array, not a single event
        // so TS is mistaken about the lack of an iterator method...
        (events: any) => {
            let correctlyTypedEvents: DebouncedEvent[] = events;
            console.log("here are the events:");
            console.debug(correctlyTypedEvents);
            let shouldReload = false;
            for (let { path } of correctlyTypedEvents) {
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

function findCommonPrefix(words: string[]): string {
    // check border cases size 1 array and empty first word)
    if (!words[0] || words.length == 1) return words[0] || "";
    let i = 0;
    // while all words have the same character at position i, increment i
    while (words[0][i] && words.every(w => w[i] === words[0][i])) {
        i++;
    }
    // prefix is the substring from the beginning to the last successfully checked i
    return words[0].slice(0, i);
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
    // out of all the paths in the text box, this is the one whose cluster should be displayed
    const [pathToDisplayOnceRead, setPathToDisplayOnceRead] = useState<string | undefined>(undefined);
    const stopCallbacks = useRef<(() => void)[]>([]);
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
        const newStopcallbacks = [];
        try {
            // an object whose type cannot be expressed with TS
            // keys are paths...
            const association: any = await invoke('associate_parents_children',
                { paths: separatePaths.join(";") }
            );
            const association_map: Map<string, string[]> = new Map(Object.entries(association));
            for (const [parent, children] of association_map.entries()) {
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
        let svgs: [string, Lv1ReadResult][] = await invoke('read_contents',
            { paths: separatePaths.join(";") });
        let newReadResults = new Map<string, Lv1ReadResult>();
        svgs.forEach((pair) => { newReadResults.set(pair[0], pair[1]); });
        setReadResults(newReadResults);
    }

    async function checkLearningPath() {
        console.debug(learningPath.split(new RegExp("\\s+")).filter((e) => e !== ""));
        let resultOfCheck: string[] = await invoke('check_learning_path_stateful', {
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
        else if (pathToDisplayOnceRead && !separatePaths.includes(pathToDisplayOnceRead)) {
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

    const onOptionChange: React.ChangeEventHandler<HTMLInputElement> = (e) => {
        setPathToDisplayOnceRead(e.target.value);
    }

    const buttonSection =
        loading ?
            <p>Please hold</p> :
            <div className="row">
                {
                    /* Could split into separate component? */
                    Array.from(readResults.entries())
                        .map(([k, v]) => {
                            let icon = "✅";
                            if (v.Err) {
                                icon = "✖️"
                            }
                            else if (v.Ok && v.Ok.length > 0 && v.Ok[0].length > 0) {
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
    const displayedResult = pathToDisplayOnceRead && readResults.get(pathToDisplayOnceRead);
    const readResultSection =
        displayedResult ?
            (loading ?
                <p>Loading</p> :
                <ReadResult
                    value={displayedResult} />) :
            <p>Cannot display anything with this input.</p>

    return (
        <>
            <div className="container">
                <TextareaAutosize
                    id="files-input"
                    onChange={(e) => setPaths(e.currentTarget.value)}
                    placeholder="Enter &quot;;&quot;-separated paths"
                />

                {buttonSection}
                {readResultSection}
                {
                    loading || pathToDisplayOnceRead !== COMPLETE_GRAPH_LABEL ?
                        <></> :
                        <><TextareaAutosize
                            value={learningPath}
                            placeholder="enter whitespace-separated nodes that make up a learning path"
                            onChange={async (e) => { setLearningPath(e.target.value) }} />
                            {learningPathComments.map((comment, idx) => <p key={idx}>{comment}</p>)}
                        </>
                }
                {
                    loading || pathToDisplayOnceRead !== COMPLETE_GRAPH_LABEL || learningPathComments.length > 0 || Array.from(readResults.values()).some((readResult) => readResult.Ok && readResult.Ok[0].length > 0) ?
                        <></> :
                        <button disabled={zipping} onClick={async (_) => { setZipping(true); let zipResult = await invoke('build_zip', { paths }); console.debug(zipResult); setZipping(false); }}>Zip it!</button>
                }
            </div>
        </>
    );
}

export default App;
