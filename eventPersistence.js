/**
 * This script provides the JavaScript implementation for the persistence
 * ports defined in the corresponding Event.elm module. It uses IndexedDB
 * for storage and a BroadcastChannel for multi-tab synchronization.
 *
 * @param {object} elmApp - The initialized Elm application instance.
 */
function setupPersistence(elmApp) {
    const DB_NAME = "event-sourcing-db";
    const DB_VERSION = 1;
    const EVENTS_STORE = "events";
    const COUNTERS_STORE = "stream_counters";
  
    // Used to notify other tabs of database changes.
    const channel = new BroadcastChannel("event_db_updates");
  
    let db;
  
    /**
     * Initializes the IndexedDB database and its object stores.
     * @returns {Promise<IDBDatabase>} A promise that resolves with the database instance.
     */
    function initDB() {
      return new Promise((resolve, reject) => {
        const request = indexedDB.open(DB_NAME, DB_VERSION);
  
        request.onupgradeneeded = (event) => {
          const dbInstance = event.target.result;
  
          // 1. Store for all event envelopes
          if (!dbInstance.objectStoreNames.contains(EVENTS_STORE)) {
            const store = dbInstance.createObjectStore(EVENTS_STORE, {
              keyPath: "eventId",
            });
            // Index for efficiently querying all events for a given stream
            store.createIndex("streamId_idx", "streamId", { unique: false });
          }
  
          // 2. Store for atomically incrementing stream IDs
          if (!dbInstance.objectStoreNames.contains(COUNTERS_STORE)) {
            dbInstance.createObjectStore(COUNTERS_STORE, {
              keyPath: "streamType",
            });
          }
        };
  
        request.onsuccess = (event) => {
          db = event.target.result;
          console.log("IndexedDB initialized successfully.");
          resolve(db);
        };
  
        request.onerror = (event) => {
          console.error("IndexedDB error:", event.target.error);
          reject(event.target.error);
        };
      });
    }
  
    /**
     * Sends a formatted result back to the Elm application.
     * @param {object} result - The result object to send.
     */
    function sendResultToElm(result) {
        // This port must be defined in your Elm application to receive the outcome.
        if (elmApp.ports.persistenceResultSub) {
          console.log("Sending result to Elm:", result);
          elmApp.ports.persistenceResultSub.send(result);
        } else {
          // This was the source of the confusing warning.
          console.warn("Elm port 'persistenceResultSub' is not defined. Cannot send persistence result.");
        }
      }
  
    /**
     * Formats a successful result for Elm.
     * @param {object} envelope - The successfully persisted event envelope.
     * @returns {object}
     */
    function formatSuccess(envelope) {
      return { ok: true, value: envelope };
    }
  
    /**
     * Formats an error result for Elm.
     * @param {string} tag - The error tag (e.g., "ConcurrencyError").
     * @param {string[]} args - Arguments for the error constructor.
     * @returns {object}
     */
    function formatError(tag, args = []) {
      return { ok: false, value: { tag, args } };
    }
  
  
    // ============================================================================
    //  PORT: hydrateStreamCmd (for loading a stream)
    // ============================================================================
    if (elmApp.ports.hydrateStreamCmd) {
      elmApp.ports.hydrateStreamCmd.subscribe((info) => {
        if (!db) {
          console.error("hydrateStreamCmd: Database not initialized.");
          return;
        }
        console.log("hydrateStreamCmd received info:", info);
        const { streamId } = info;
        const tx = db.transaction(EVENTS_STORE, "readonly");
        const store = tx.objectStore(EVENTS_STORE);
        const index = store.index("streamId_idx");
        const request = index.getAll(streamId);
  
        request.onsuccess = () => {
          console.log(`Hydrated ${request.result.length} events for stream ${streamId}`);
          if (elmApp.ports.eventsSubscription) {
            console.log("Sending hydrated events to Elm:", request.result);
            elmApp.ports.eventsSubscription.send(request.result);
          } else {
            console.warn("Elm port 'eventsSubscription' is not defined. Cannot send hydrated events.");
          }
        };
  
        request.onerror = () => {
          console.error("Error hydrating stream:", request.error);
        };
      });
    }


    // ============================================================================
    //  PORT: persistEventCmd (for existing streams)
    // ============================================================================
    if (elmApp.ports.persistEventCmd) {
      elmApp.ports.persistEventCmd.subscribe((envelope) => {
        console.log("persistEventCmd received envelope:", envelope);
        const tx = db.transaction(EVENTS_STORE, "readwrite");
        const store = tx.objectStore(EVENTS_STORE);
        const index = store.index("streamId_idx");
  
        // 1. Get all existing events for this stream to check the latest position.
        const request = index.getAll(envelope.streamId);
  
        request.onsuccess = () => {
          const existingEvents = request.result;
          let currentPosition = 0;
          if (existingEvents.length > 0) {
            // Find the highest streamPosition among the events.
            currentPosition = Math.max(...existingEvents.map(e => e.streamPosition));
          }
  
          // 2. Perform the optimistic concurrency check.
          if (currentPosition !== envelope.streamPosition - 1) {
            const error = formatError("ConcurrencyError");
            console.error("Concurrency check failed.", { expected: envelope.streamPosition - 1, actual: currentPosition });
            sendResultToElm(error);
            tx.abort();
            return;
          }
  
          // 3. If the check passes, add the new event.
          store.add(envelope);
        };
  
        tx.oncomplete = () => {
          console.log("Event persisted:", envelope.eventId);
          sendResultToElm(formatSuccess(envelope));
          // Notify other tabs that this stream has been updated.
          channel.postMessage({ streamId: envelope.streamId });
        };
  
        tx.onerror = () => {
          console.error("Transaction error in persistEventCmd:", tx.error);
          sendResultToElm(formatError("TransactionError", [tx.error.message]));
        };
      });
    }
  
  
    // ============================================================================
    //  PORT: persistNewEventCmd (for new streams)
    // ============================================================================
    if (elmApp.ports.persistNewEventCmd) {
      elmApp.ports.persistNewEventCmd.subscribe(({ streamType, envelope }) => {
        // This transaction MUST cover both stores to be atomic.
        const tx = db.transaction([COUNTERS_STORE, EVENTS_STORE], "readwrite");
        const countersStore = tx.objectStore(COUNTERS_STORE);
        const eventsStore = tx.objectStore(EVENTS_STORE);
        let finalEnvelope;
  
        // 1. Get the current counter for the given stream type.
        const counterRequest = countersStore.get(streamType);
  
        counterRequest.onsuccess = () => {
          const counter = counterRequest.result;
          const newCount = counter ? counter.count + 1 : 1;
  
          // 2. Update the counter value.
          countersStore.put({ streamType: streamType, count: newCount });
  
          // 3. Construct the final envelope with the generated ID and position.
          finalEnvelope = {
            ...envelope,
            streamId: `${streamType}-${newCount}`,
            streamPosition: 1,
          };
  
          // 4. Add the new event to the events store.
          eventsStore.add(finalEnvelope);
        };
  
        tx.oncomplete = () => {
          console.log("New stream event persisted:", finalEnvelope.eventId);
          sendResultToElm(formatSuccess(finalEnvelope));
          // Notify other tabs that a new stream was created.
          channel.postMessage({ streamId: finalEnvelope.streamId });
        };
  
        tx.onerror = () => {
          console.error("Transaction error in persistNewEventCmd:", tx.error);
          sendResultToElm(formatError("TransactionError", [tx.error.message]));
        };
      });
    }
  
    // ============================================================================
    //  MULTI-TAB SYNCHRONIZATION
    // ============================================================================
    if (elmApp.ports.dbUpdateNotification) {
      channel.onmessage = (event) => {
        console.log("Received DB update notification from another tab for stream:", event.data.streamId);
        // Notify the Elm app that a stream has been updated elsewhere,
        // so it can decide whether to refetch the data.
        elmApp.ports.dbUpdateNotification.send(event.data.streamId);
      };
    }
  
    // Initialize the database and start the process.
    initDB().catch(err => {
      // If the DB can't be opened, we should inform Elm.
      // This requires a way to send an initial error, which might need another port
      // or a flag passed during initialization.
      console.error("Failed to initialize persistence layer.", err);
    });
  }