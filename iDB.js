let connectIndexedDB = (app) => {
    let db;
    let request = indexedDB.open("FlexFoodLog", 8);
    request.onerror = (event) => {
        console.error("Database error: " + event.target.errorCode);
    };
    request.onsuccess = (event) => {
        db = event.target.result;

        console.log("Database initialized");
    };

    function runReq(req) {
        return new Promise((resolve, reject) => {
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error || new Error("IDB request failed"));
        });
    }

    function runTx(tx) {
        return new Promise((resolve, reject) => {
            tx.oncomplete = () => resolve();
            tx.onabort = () => reject(tx.error || new Error("IDB transaction aborted"));
            tx.onerror = () => reject(tx.error || new Error("IDB transaction error"));
        });
    }

    let updateIndexes = async (req, storeName, indexes) => {
        let tx = req.transaction;
        let store = tx.objectStore(storeName);
        for (let idx of indexes)
            if (!store.indexNames.contains(idx))
                store.createIndex(idx, idx, { unique: false });
    };

    // -- Initialize the database schema if needed --
    request.onupgradeneeded = async (event) => {
        db = event.target.result;
        let objectStore;
        if (!db.objectStoreNames.contains("events")) {
            objectStore = db.createObjectStore("events", { keyPath: "id", autoIncrement: true });
        }
        updateIndexes(request, "events", ["streamId", "timestamp", "type"]).catch((err) => { });

        if (!db.objectStoreNames.contains("streams")) {
            objectStore = db.createObjectStore("streams", { keyPath: "id", autoIncrement: true });
        }
        updateIndexes(request, "streams", ["name"]).catch((err) => { });

        console.log("Database schema (re-)initialized");
    }

    // -- Now db should be initialized --



    app.ports.saveEvent.subscribe(async (envelope) => {
        if (!db) {
            console.error("Database not initialized");
            return;
        }
        // Start a transaction
        let tx = db.transaction(["events", "streams"], "readwrite");
        // Get object stores
        let eventStore = tx.objectStore("events");
        let streamStore = tx.objectStore("streams");

        // Check if a new streamId is needed
        let requiresStreamId = envelope.streamId.slice(-2) === ":*";
        if (requiresStreamId) {
            // Extract base stream name and get or create streamId
            let streamName = envelope.streamId.slice(0, -2);
            let getRequest = await runReq(streamStore.index("name").get(streamName));
            let streamId;
            if (getRequest) {
                // Stream exists, increment nextId
                streamId = getRequest.nextId;
                await runReq(streamStore.put({ id: getRequest.id, name: streamName, nextId: streamId + 1 }));
            } else {
                // Stream does not exist, create it
                streamId = 1;
                await runReq(streamStore.add({ name: streamName, nextId: 2 }));
            }
            envelope.streamId = envelope.streamId.replace("*", streamId);
        }
        await runReq(eventStore.add(envelope));
        await runTx(tx);
    });


    // Filter events by type
    app.ports.queryEventType.subscribe(async (type) => {
        if (!db) {
            console.error("Database not initialized");
            return;
        }
        console.log("Querying events of type:", type);
        let tx = db.transaction("events", "readonly");
        let eventStore = tx.objectStore("events");
        let results = [];
        let index = eventStore.index("type");
        let range = IDBKeyRange.only(type);
        let cursorRequest = index.openCursor(range);
        cursorRequest.onsuccess = (event) => {
            let cursor = event.target.result;
            if (cursor) {
                results.push(cursor.value);
                cursor.continue();
            }
            console.log("Found events:", results);
            app.ports.onEvents.send(results);
        };

        await runTx(tx);
    });

    app.ports.queryStreamEvents.subscribe(async (streamId) => {
        if (!db) {
            console.error("Database not initialized");
            return;
        }
        let tx = db.transaction("events", "readonly");
        let eventStore = tx.objectStore("events");
        let results = [];
        let index = eventStore.index("streamId");

        let range = IDBKeyRange.only(streamId);
        let cursorRequest = index.openCursor(range);
        cursorRequest.onsuccess = (event) => {
            let cursor = event.target.result;
            if (cursor) {
                results.push(cursor.value);
                cursor.continue();
            }
        };
        await runTx(tx);
        app.ports.onEvents.send(results);

    });

    app.ports.queryAllEvents.subscribe(async () => {
        if (!db) {
            console.error("Database not initialized");
            return;
        }
        let tx = db.transaction("streams", "readonly");
        let streamStore = tx.objectStore("streams");
        let results = [];
        let cursorRequest = streamStore.openCursor();
        cursorRequest.onsuccess = (event) => {
            let cursor = event.target.result;
            if (cursor) {
                results.push(cursor.value);
                cursor.continue();
            }
        };
        await runTx(tx);
        app.ports.onEvents.send(results);
    });



}