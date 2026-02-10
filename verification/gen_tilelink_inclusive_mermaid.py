import sys
import re
import os

def parse_log(filepath):
    events = []
    
    # Regex patterns
    # Updated to optionally capture beat/last for A/B/C channels as well
    p_inner_a = re.compile(r'L2 bank=(\d+) INNER.A opcode=(\w+) param=(\d+) size=(\d+) source=\s*(\d+) address=(0x[\da-f]+) tag=(0x[\da-f]+) set=\s*(\d+)(?: beat=(\d+) last=(\d+))?')
    p_inner_b = re.compile(r'L2 bank=(\d+) INNER.B opcode=(\w+) param=(\d+) size=(\d+) source=\s*(\d+) address=(0x[\da-f]+) tag=(0x[\da-f]+) set=\s*(\d+)(?: beat=(\d+) last=(\d+))?')
    p_inner_c = re.compile(r'L2 bank=(\d+) INNER.C opcode=(\w+) param=(\d+) size=(\d+) source=\s*(\d+) address=(0x[\da-f]+) tag=(0x[\da-f]+) set=\s*(\d+)(?: beat=(\d+) last=(\d+))?')
    p_inner_d = re.compile(r'L2 bank=(\d+) INNER.D opcode=(\w+) param=(\d+) size=(\d+) source=\s*(\d+) sink=(\d+) beat=(\d+) last=(\d+)')
    p_inner_e = re.compile(r'L2 bank=(\d+) INNER.E opcode=(\w+) sink=(\d+)')
    
    p_outer_a = re.compile(r'L2 bank=(\d+) OUTER.A opcode=(\w+) param=(\d+) size=(\d+) source=(\d+) address=(0x[\da-f]+) tag=(0x[\da-f]+) set=\s*(\d+)(?: beat=(\d+) last=(\d+))?')
    p_outer_c = re.compile(r'L2 bank=(\d+) OUTER.C opcode=(\w+) param=(\d+) size=(\d+) source=(\d+) address=(0x[\da-f]+) tag=(0x[\da-f]+) set=\s*(\d+)(?: beat=(\d+) last=(\d+))?')
    p_outer_d = re.compile(r'L2 bank=(\d+) OUTER.D opcode=(\w+) param=(\d+) size=(\d+) source=(\d+) sink=(\d+) beat=(\d+) last=(\d+)')

    p_outer_e = re.compile(r'L2 bank=(\d+) OUTER.E opcode=(\w+) sink=(\d+) beat=(\d+) last=(\d+)')
    
    p_primary_miss = re.compile(r'\[SSBC MSHR (\d+)\] PRIMARY_MISS origSet=\s*(\d+) origWay=(\d+) origTag=(0x[\da-f]+) scBit=(\d+)')
    p_primary_hit = re.compile(r'\[SSBC MSHR (\d+)\] PRIMARY_HIT origSet=\s*(\d+) origWay=(\d+) origTag=(0x[\da-f]+)')

    p_secondary_miss = re.compile(r'\[SSBC MSHR (\d+)\] SECONDARY_MISS origSet=\s*(\d+)(?:\s+partnerSet=\s*(\d+) partnerWay=(\d+) partnerTag=(0x[\da-f]+))?')
    p_secondary_hit = re.compile(r'\[SSBC MSHR (\d+)\] SECONDARY_HIT origSet=\s*(\d+)(?:\s+partnerSet=\s*(\d+) partnerWay=(\d+) partnerTag=(0x[\da-f]+))?')
    
    p_dir_write = re.compile(r'\[SSBC Directory\] DIR_WRITE set=\s*(\d+) way=(\d+) tag=(0x[\da-f]+) state=(\d+) dirty=(\d+) displaced=(\d+) originSet=\s*(\d+)')
    
    p_migrate_trigger = re.compile(r'\[SSBC MSHR (\d+)\] MIGRATE_TRIGGER srcSet=\s*(\d+) srcTag=(0x[\da-f]+) -> partnerSet=\s*(\d+) partnerWay=(\d+)')
    p_partner_lookup = re.compile(r'\[SSBC MSHR (\d+)\] PARTNER_LOOKUP partnerSet=\s*(\d+) way=(\d+) tag=(0x[\da-f]+) dirty=(\d+) valid=(\d+)')

    try:
        with open(filepath, 'r') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()
                if not line: continue
                
                if "[InclusiveCache]" not in line:
                    continue
                
                # Check for tag
                
                m = p_inner_a.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'INNER_A',
                        'opcode': m.group(2),
                        'address': m.group(6),
                        'tag': m.group(7),
                        'set': m.group(8)
                    }
                    if m.group(9): event['beat'] = m.group(9)
                    if m.group(10): event['last'] = m.group(10)
                    events.append(event)
                    continue

                m = p_inner_b.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'INNER_B',
                        'opcode': m.group(2),
                        'address': m.group(6),
                        'tag': m.group(7),
                        'set': m.group(8)
                    }
                    if m.group(9): event['beat'] = m.group(9)
                    if m.group(10): event['last'] = m.group(10)
                    events.append(event)
                    continue

                m = p_inner_c.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'INNER_C',
                        'opcode': m.group(2),
                        'address': m.group(6),
                        'tag': m.group(7),
                        'set': m.group(8)
                    }
                    if m.group(9): event['beat'] = m.group(9)
                    if m.group(10): event['last'] = m.group(10)
                    events.append(event)
                    continue

                m = p_inner_d.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'INNER_D',
                        'opcode': m.group(2),
                        'beat': m.group(7),
                        'last': m.group(8)
                    })
                    continue

                m = p_inner_e.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'INNER_E',
                        'opcode': m.group(2)
                    })
                    continue

                m = p_outer_a.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'OUTER_A',
                        'opcode': m.group(2),
                        'tag': m.group(7),
                        'set': m.group(8)
                    }
                    if m.group(9): event['beat'] = m.group(9)
                    if m.group(10): event['last'] = m.group(10)
                    events.append(event)
                    continue

                m = p_outer_c.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'OUTER_C',
                        'opcode': m.group(2),
                        'tag': m.group(7),
                        'set': m.group(8)
                    }
                    if m.group(9): event['beat'] = m.group(9)
                    if m.group(10): event['last'] = m.group(10)
                    events.append(event)
                    continue

                m = p_outer_d.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'OUTER_D',
                        'opcode': m.group(2),
                        'beat': m.group(7),
                        'last': m.group(8)
                    })
                    continue

                m = p_outer_e.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'OUTER_E',
                        'opcode': m.group(2)
                    })
                    continue

                m = p_primary_miss.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'PRIMARY_MISS',
                        'mshr': m.group(1),
                        'set': m.group(2),
                        'way': m.group(3),
                        'tag': m.group(4)
                    })
                    continue

                m = p_primary_hit.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'PRIMARY_HIT',
                        'mshr': m.group(1),
                        'set': m.group(2),
                        'way': m.group(3),
                        'tag': m.group(4)
                    })
                    continue

                m = p_secondary_miss.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'SECONDARY_MISS',
                        'mshr': m.group(1),
                        'set': m.group(2)
                    }
                    if m.group(3):
                        event['partnerSet'] = m.group(3)
                        event['partnerWay'] = m.group(4)
                        event['partnerTag'] = m.group(5)
                    events.append(event)
                    continue

                m = p_secondary_hit.search(line)
                if m:
                    event = {
                        'line': line_num,
                        'type': 'SECONDARY_HIT',
                        'mshr': m.group(1),
                        'set': m.group(2)
                    }
                    if m.group(3):
                        event['partnerSet'] = m.group(3)
                        event['partnerWay'] = m.group(4)
                        event['partnerTag'] = m.group(5)
                    events.append(event)
                    continue

                m = p_dir_write.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'DIR_WRITE',
                        'set': m.group(1),
                        'way': m.group(2),
                        'tag': m.group(3)
                    })
                    continue

                m = p_migrate_trigger.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'MIGRATE_TRIGGER',
                        'mshr': m.group(1),
                        'srcSet': m.group(2),
                        'srcTag': m.group(3),
                        'dstSet': m.group(4),
                        'dstWay': m.group(5)
                    })
                    continue
                
                m = p_partner_lookup.search(line)
                if m:
                    events.append({
                        'line': line_num,
                        'type': 'PARTNER_LOOKUP',
                        'mshr': m.group(1),
                        'set': m.group(2),
                        'way': m.group(3),
                        'tag': m.group(4)
                    })
                    continue

    except FileNotFoundError:
        print(f"Error: File {filepath} not found.")
        sys.exit(1)

    return events

def combine_bursts(events):
    """
    Combines consecutive events that form a burst into a single event with 'burst_len' property.
    This simplifies the Mermaid diagram by showing one arrow for the whole burst.
    Also handles implicit bursts for C-channel events that lack explicit beat info.
    """
    if not events: return []
    
    combined = []
    current_burst = [events[0]]
    
    for i in range(1, len(events)):
        prev = current_burst[-1]
        curr = events[i]
        
        combinable = True
        
        # Must be same type and opcode
        if prev['type'] != curr['type']: 
            combinable = False
        elif 'opcode' not in prev:
            combinable = False
        elif prev['opcode'] != curr['opcode']: 
            combinable = False
        else:
             # Check metadata consistency
             for k in ['source', 'sink', 'param', 'size', 'address', 'tag', 'set']:
                 if k in prev and k in curr and prev[k] != curr[k]:
                     combinable = False
                     break
                 if (k in prev) != (k in curr):
                     combinable = False
                     break
            
             # Check beat continuity
             if 'beat' in prev and 'beat' in curr:
                 # Explicit beats (D channel usually)
                 if int(curr['beat']) != int(prev['beat']) + 1:
                     combinable = False
                 # Previous beat must not have been 'last'
                 if 'last' in prev and int(prev['last']) == 1:
                     combinable = False
             elif 'beat' not in prev and 'beat' not in curr:
                 # Implicit bursts (C channel)
                 # Only combine if it's a data-carrying message
                 is_burstable = (prev['type'] in ['INNER_C', 'OUTER_C']) and ('Data' in prev['opcode'])
                 if not is_burstable:
                     combinable = False
             else:
                 # Mixed explicit/implicit - should not happen for same opcode
                 combinable = False

        if combinable:
            current_burst.append(curr)
        else:
            # Commit
            first = current_burst[0]
            if len(current_burst) > 1:
                first['burst_len'] = len(current_burst)
                
                if 'beat' in first:
                    first['burst_start'] = first['beat']
                    first['burst_end'] = current_burst[-1]['beat']
                else:
                    # Implicit beats (0 to N-1)
                    first['burst_start'] = 0
                    first['burst_end'] = len(current_burst) - 1
                
                # Check if the last beat was marked as last
                last_event = current_burst[-1]
                if 'last' in last_event:
                    first['burst_has_last'] = (int(last_event['last']) == 1)
                else:
                    # Implicit bursts assumed to end with last
                    first['burst_has_last'] = True
            
            combined.append(first)
            current_burst = [curr]
            
    if current_burst:
        first = current_burst[0]
        if len(current_burst) > 1:
             first['burst_len'] = len(current_burst)
             if 'beat' in first:
                 first['burst_start'] = first['beat']
                 first['burst_end'] = current_burst[-1]['beat']
             else:
                 first['burst_start'] = 0
                 first['burst_end'] = len(current_burst) - 1
             
             last_event = current_burst[-1]
             if 'last' in last_event:
                 first['burst_has_last'] = (int(last_event['last']) == 1)
             else:
                 first['burst_has_last'] = True

        combined.append(first)
        
    return combined

def generate_mermaid(events, output_file):
    header = "```mermaid\n    sequenceDiagram\n    participant Inner as Inner (L1/Core)\n    participant L2_SSBC as L2 Cache & SSBC Logic\n    participant Outer as Outer (Memory/L3)\n\n"
    footer = "```\n"
    
    max_lines = 500  # Threshold to split diagrams
    current_lines = 0
    transaction_count = 0
    
    with open(output_file, 'w') as f:
        f.write(header)
        
        for event in events:
            lines_to_write = []
            line_str = f"L{event['line']}"
            
            def get_beat_str(evt):
                if 'burst_len' in evt:
                    s = f"(Beat {evt['burst_start']}-{evt['burst_end']}"
                    if evt.get('burst_has_last'):
                        s += ", Last"
                    s += ")"
                    return s
                elif 'beat' in evt:
                    if int(evt['beat']) == 0:
                        return f"(Beat {evt['beat']})"
                    elif int(evt['last']) == 1:
                        return f"(Beat {evt['beat']} - Last)"
                return ""

            if event['type'] == 'INNER_A':
                transaction_count += 1
                lines_to_write.append(f"    Note over Inner, L2_SSBC: [{line_str}] Transaction {transaction_count}: {event['opcode']} (Set: {event['set']}, Tag: {event['tag']})\n")
                lines_to_write.append(f"    Inner->>L2_SSBC: {event['opcode']} (Addr: {event['address']}, Set: {event['set']}, Tag: {event['tag']})\n")
            
            elif event['type'] == 'INNER_B':
                lines_to_write.append(f"    L2_SSBC->>Inner: {event['opcode']} (Addr: {event['address']}, Set: {event['set']}, Tag: {event['tag']}) [{line_str}]\n")

            elif event['type'] == 'INNER_C':
                # Treat Release or ReleaseData as new transaction boundary
                if (event['opcode'] in ['Release', 'ReleaseData'] and event.get('beat') == '0'):
                    transaction_count += 1
                    lines_to_write.append(f"    Note over Inner, L2_SSBC: [{line_str}] Transaction {transaction_count}: {event['opcode']} (Set: {event['set']}, Tag: {event['tag']})\n")
                
                info = f"{event['opcode']} (Addr: {event['address']}, Set: {event['set']}, Tag: {event['tag']})"
                beat_info = get_beat_str(event)
                if beat_info:
                    lines_to_write.append(f"    Inner->>L2_SSBC: {info} {beat_info} [{line_str}]\n")
                else:
                    lines_to_write.append(f"    Inner->>L2_SSBC: {info} [{line_str}]\n")

            elif event['type'] == 'PRIMARY_MISS':
                lines_to_write.append(f"    L2_SSBC->>L2_SSBC: [{line_str}] Primary Miss [MSHR {event['mshr']}] (victim : Set: {event['set']}, Tag: {event['tag']} | Way: {event['way']})\n")
            
            elif event['type'] == 'PRIMARY_HIT':
                lines_to_write.append(f"    L2_SSBC->>L2_SSBC: [{line_str}] Primary Hit [MSHR {event['mshr']}] (Set {event['set']} Way {event['way']} | Tag {event['tag']})\n")
            
            elif event['type'] == 'SECONDARY_MISS':
                msg = f"Secondary Miss [MSHR {event['mshr']}] (Set {event['set']})"
                if 'partnerSet' in event:
                    msg += f" | In Partner (victim : Set: {event['partnerSet']}, Tag: {event['partnerTag']} | Way: {event['partnerWay']})"
                lines_to_write.append(f"    L2_SSBC->>L2_SSBC: [{line_str}] {msg}\n")

            elif event['type'] == 'SECONDARY_HIT':
                msg = f"Secondary Hit [MSHR {event['mshr']}] (Set {event['set']})"
                if 'partnerSet' in event:
                    msg += f" | Partner: Set {event['partnerSet']} Way {event['partnerWay']} Tag {event['partnerTag']}"
                lines_to_write.append(f"    L2_SSBC->>L2_SSBC: [{line_str}] {msg}\n")

            elif event['type'] == 'MIGRATE_TRIGGER':
                lines_to_write.append(f"    Note over L2_SSBC, Outer: [{line_str}] Migration Triggered [MSHR {event['mshr']}]\n")
                lines_to_write.append(f"    L2_SSBC->>L2_SSBC: Migrate Tag {event['srcTag']} (Set {event['srcSet']}) -> Set {event['dstSet']} Way {event['dstWay']}\n")

            elif event['type'] == 'PARTNER_LOOKUP':
                 lines_to_write.append(f"    L2_SSBC->>L2_SSBC: [{line_str}] Partner Lookup [MSHR {event['mshr']}] (Set {event['set']}) found Way {event['way']} Tag {event['tag']})\n")

            elif event['type'] == 'OUTER_A':
                lines_to_write.append(f"    L2_SSBC->>Outer: [{line_str}] {event['opcode']} (Tag {event['tag']}, Set {event['set']})\n")

            elif event['type'] == 'OUTER_C':
                info = f"{event['opcode']} (Tag {event['tag']}, Set {event['set']})"
                beat_info = get_beat_str(event)
                if beat_info:
                    lines_to_write.append(f"    L2_SSBC->>Outer: [{line_str}] {info} {beat_info}\n")
                else:
                    lines_to_write.append(f"    L2_SSBC->>Outer: [{line_str}] {info}\n")

            elif event['type'] == 'OUTER_D':
                info = f"{event['opcode']}"
                beat_info = get_beat_str(event)
                if beat_info:
                    lines_to_write.append(f"    Outer-->>L2_SSBC: {info} {beat_info} [{line_str}]\n")
                else:
                    lines_to_write.append(f"    Outer-->>L2_SSBC: {info} [{line_str}]\n")

            elif event['type'] == 'OUTER_E':
                lines_to_write.append(f"    L2_SSBC->>Outer: [{line_str}] {event['opcode']}\n")

            elif event['type'] == 'INNER_D':
                 info = f"{event['opcode']}"
                 beat_info = get_beat_str(event)
                 if beat_info:
                     lines_to_write.append(f"    L2_SSBC-->>Inner: {info} {beat_info} [{line_str}]\n")
                 else:
                     lines_to_write.append(f"    L2_SSBC-->>Inner: {info} [{line_str}]\n")

            elif event['type'] == 'INNER_E':
                lines_to_write.append(f"    Inner->>L2_SSBC: [{line_str}] {event['opcode']}\n")

            elif event['type'] == 'DIR_WRITE':
                lines_to_write.append(f"    Note right of L2_SSBC: [{line_str}] Meta Update (Set {event['set']} Way {event['way']} Tag {event['tag']})\n")

            # Check if adding these lines would exceed the limit
            if current_lines + len(lines_to_write) > max_lines:
                f.write(footer)
                f.write("\n")
                f.write(header)
                current_lines = 0
            
            for line in lines_to_write:
                f.write(line)
            current_lines += len(lines_to_write)

        f.write(footer)
    print(f"Generated {output_file}")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 gen_tilelink_inclusive_mermaid.py <logfile>")
        sys.exit(1)
    
    logfile = sys.argv[1]
    events = parse_log(logfile)
    events = combine_bursts(events)
    generate_mermaid(events, "tilelink_flow.md")
