import "windows"
import "allocator"

const Buffer = struct {
    span: String
    allocator: Allocator
}

fn create_buffer(size: Int, alignment: Int = 8, is_zeroed: Bool = false): Buffer => {
    var result: Buffer
    result.allocator = current_allocator
    let allocation = result.allocator.allocate(Allocation(size = size, alignment = alignment, is_zeroed = is_zeroed))
    result.span.data = allocation.data
    result.span.count = allocation.size
    return result
}

fn free(buffer: *Buffer): None => {
    buffer.allocator.free(Allocation(data = buffer.span.data, size = buffer.span.count))
    *buffer = none as Buffer
}

const File = HANDLE

const OpenFileOptions = struct {
    read: Bool
    write: Bool
}

fn open_file(path: String, options: OpenFileOptions): File => {
    return CreateFileA(path.data, GENERIC_READ, FILE_SHARE_READ, none, OPEN_EXISTING, 0, none)
}

// If anything goes wrong, returns an empty buffer
// TODO: return option
fn read_entire_file(path: String, out_result: *var Buffer): Bool => {
    var ok: Bool = true

    var file = CreateFileA(path.data, GENERIC_READ, FILE_SHARE_READ, none, OPEN_EXISTING, 0, none)
    defer CloseHandle(file)

    SetFilePointerEx(file, 0, 0, FILE_END);

	var file_size: S64
	SetFilePointerEx(file, 0, &file_size, FILE_CURRENT)

    SetFilePointerEx(file, 0, 0, FILE_BEGIN);

    var result = create_buffer(file_size)
    defer if !ok then buffer.free()

    const max_read_size = 0xffff_ffff
    var bytes_remaining = file_size
    var dest = result.span.data
    var bytes_read: DWORD
    while bytes_remaining >= max_read_size {
        if !ReadFile(file, dest, max_read_size, &bytes_read, 0) {
            ok = false
            return ok
        }
        bytes_remaining -= max_read_size
        dest += max_read_size
    }
    if !ReadFile(file, dest, bytes_remaining, &bytes_read, 0) {
        ok = false
        return ok
    }

    *out_result = result
    ok = true
    return ok
}

// TODO
// private

const WinOpenFileOptions = struct {
	access: DWORD
	share: DWORD
	creation: DWORD
}

fn get_win32_options(options: OpenFileOptions): WinOpenFileOptions => {
	var result: WinOpenFileOptions;
	if (options.read && options.write) {
		result.access = GENERIC_READ | GENERIC_WRITE;
		result.share = 0;
		result.creation = OPEN_ALWAYS;
	} else if (options.read) {
		result.access = GENERIC_READ;
		result.share = FILE_SHARE_READ;
		result.creation = OPEN_EXISTING;
	} else if (options.write) {
		result.access = GENERIC_WRITE;
		result.share = 0;
		result.creation = CREATE_ALWAYS;
	} else {
		result.access = 0;
		result.share = FILE_SHARE_READ | FILE_SHARE_WRITE;
		result.creation = OPEN_EXISTING;
	}
	return result;
}