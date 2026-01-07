**Structuring SSBC algorithm steps**

I need to break down the SSBC algorithm into clear steps. First, I’ll define the saturation counter behaviour and its range. Then, I’ll explain the association algorithm, where the partner set index is determined by MSB complement. For displacement, I’ll outline when and what gets displaced, and where it’s inserted. The search algorithm will follow, detailing the primary and secondary searches. I’ll also mention invariants and thresholds, like the saturation counter range and displacement limit. Finally, I’ll structure the steps for each access, from metadata to maintaining the saturation counter bit.
You’ve got the full story in the paper—let’s turn it into a clean, **step‑by‑step algorithm** for the **Static Set Balancing Cache (SSBC)**, with no programming details.

---

### 1. Define per-set and per-line state

1. **For each cache line:**
   - **Tag**
   - **Valid bit**
   - **Displaced bit `d`:**
     - `d = 0`: line is native to this set.
     - `d = 1`: line was displaced here from its associated set.
   - **Replacement state** (e.g., LRU position).

2. **For each cache set:**
   - **Saturation counter**:
     - Range: from `0` to `2K − 1`, where `K` is associativity.
     - **Increment** on a miss to this set.
     - **Decrement** on a hit in this set.
   - **Second search bit `sc`:**
     - `sc = 1`: its associated set may hold displaced lines from this set.
     - `sc = 0`: no displaced lines from this set are currently in its associated set.

3. **Association rule (static pairing):**
   - Each set has exactly **one associated partner set**.
   - Partner index is obtained by **complementing the most significant index bit** (farthest set in index space).
   - Association is **symmetric**: if X is associated with Y, Y is associated with X.

---

### 2. Saturation counter behavior

For every access to a set:

1. **On a hit in that set:**
   - Decrement its saturation counter by 1, down to a minimum of 0.

2. **On a miss mapped to that set:**
   - Increment its saturation counter by 1, up to a maximum of `2K − 1`.

3. **Interpretation:**
   - **Low saturation:** counter in `[0, K−1]` → set’s working set likely fits.
   - **High saturation:** counter at `2K − 1` → set is under strong pressure.
   - **Displacement limit:** a set is considered a good receiver if its counter is **below `K`**.

---

### 3. Search (lookup) algorithm for each access

Given an address `A`:

1. **Determine primary set:**
   - Use the index bits of `A` to get set `i`.

2. **Primary search in set `i`:**
   - Look for a line with:
     - Matching tag.
     - Valid bit set.
     - `d = 0` (native line).
   - **If found (primary hit):**
     - Update replacement state in set `i`.
     - Decrement saturation counter of set `i`.
     - Return data and stop.

3. **Check if second search is needed:**
   - If primary search misses **and** `sc[i] = 1`:
     - Compute associated set index `i′` (partner of `i`).
     - Search set `i′` for:
       - Matching tag.
       - Valid bit set.
       - `d = 1` (displaced line).
     - **If found (secondary hit):**
       - Update replacement state in set `i′`.
       - Decrement saturation counter of set `i` (logical owner).
       - Return data and stop.

4. **If both searches miss:**
   - Treat as a **cache miss** for logical set `i`.
   - Proceed to miss handling and possible displacement.

---

### 4. Miss handling and displacement decision

On a miss mapped to set `i`:

1. **Update saturation of set `i`:**
   - Increment its saturation counter (up to `2K − 1`).

2. **Check if set `i` is “highly saturated”:**
   - Consider it **eligible to displace** only if its saturation counter is at **maximum** (`2K − 1`).

3. **Identify associated set `i′`:**
   - Use the static pairing rule (MSB complement).

4. **Check if associated set `i′` is “underutilized”:**
   - Look at saturation counter of `i′`.
   - It must be **below the displacement limit** (`< K`) to be considered a good receiver.

5. **Decide placement:**
   - **If**:
     - Saturation counter of `i` is at maximum (`2K − 1`), **and**
     - Saturation counter of `i′` is `< K`,
     - → perform a **displacement**.
   - **Else**:
     - Perform a **standard placement** in set `i` (no displacement).

---

### 5. Displacement placement (when both conditions hold)

When displacing from saturated set `i` to underutilized set `i′`:

1. **Select victim in receiver set `i′`:**
   - Choose the LRU (or equivalent) line in set `i′`.

2. **Evict victim from `i′`:**
   - Remove it from the cache (normal eviction).

3. **Move the line that would have been evicted from `i`:**
   - Instead of sending the LRU line of `i` to lower memory, **displace it to set `i′`**.
   - Insert it in set `i′` as:
     - Tag of the displaced line.
     - Valid bit set.
     - `d = 1` (displaced).
     - Replacement state set as **most recently used (MRU)**.

4. **Insert the newly fetched line in set `i`:**
   - Place the line that missed into set `i` as a normal native line:
     - Tag of the missed address.
     - Valid bit set.
     - `d = 0`.
     - Replacement state as MRU.

5. **Update second search bit:**
   - Set `sc[i] = 1` (since set `i` now has at least one line displaced into its associated set `i′`).

---

### 6. Standard placement (when displacement is not allowed)

When either `i` is not fully saturated or `i′` is not underutilized:

1. **Select victim in set `i`:**
   - Choose the LRU (or equivalent) line in set `i`.

2. **Evict victim from set `i`:**
   - Remove it from the cache (normal eviction).

3. **Insert the newly fetched line into set `i`:**
   - Tag of the missed address.
   - Valid bit set.
   - `d = 0` (native).
   - Replacement state as MRU.

4. **Maintain `sc` bits:**
   - If the evicted line from `i` had `d = 1` (i.e., it was a displaced line from its partner):
     - Check whether **any other line in the partner set still has `d = 1`** for that logical owner.
     - If **no displaced lines remain** for that owner:
       - Clear the corresponding `sc` bit for that owner set.

---

### 7. Maintaining the second search bit `sc`

For each set `i`:

1. **Set `sc[i] = 1` when:**
   - A line logically belonging to set `i` is displaced into its associated set `i′`.

2. **Clear `sc[i] = 0` when:**
   - The associated set `i′` evicts the **last** displaced line belonging to `i`.
   - This can be detected by:
     - Checking whether the OR of all `d` bits in `i′` that correspond to lines from `i` changes from `1` to `0` after an eviction.

3. **Effect on search:**
   - `sc[i] = 0` → no second search needed after a miss in set `i`.
   - `sc[i] = 1` → second search in associated set `i′` is potentially useful.

---

### 8. Core invariants to keep in mind

1. **Saturation counters:**
   - Reflect recent balance of hits/misses per set.
   - Drive when a set is considered “stressed” or “underutilized”.

2. **Displaced bit `d`:**
   - `d = 0` → line is in its natural set.
   - `d = 1` → line resides in its partner set.

3. **Second search bit `sc`:**
   - `sc[i] = 1` if and only if there exists at least one displaced line belonging to set `i` in its associated set.

4. **Displacement policy:**
   - Only displace when:
     - Source set is at maximum saturation.
     - Destination set is clearly underutilized (below displacement limit).

If you’d like, we can now walk through a concrete access trace (a few references like in Figure 2) and annotate each step with these rules so you can sanity‑check your understanding against the paper’s example.
