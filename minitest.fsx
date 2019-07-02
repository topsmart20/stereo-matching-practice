#load "Common.fs"
#load "Data.fs"
#load "Smoothness.fs"
#load "BeliefPropagation.fs"
#r "System.Memory"
#time

// let leftInput = [|
//         196uy; 180uy; 173uy; 177uy; 183uy; 182uy; 176uy; 167uy; 161uy; 154uy; 151uy; 149uy; 152uy; 154uy; 160uy; 160uy; 160uy; 159uy; 155uy; 129uy; 67uy; 40uy; 46uy; 19uy; 12uy; 31uy; 86uy; 108uy; 12uy; 62uy; 49uy; 57uy; 70uy; 65uy; 73uy; 69uy; 81uy; 159uy; 92uy; 154uy; 130uy; 35uy; 81uy; 72uy; 63uy; 73uy; 75uy; 81uy; 87uy; 89uy;
//         199uy; 186uy; 175uy; 179uy; 185uy; 184uy; 184uy; 183uy; 181uy; 179uy; 173uy; 161uy; 152uy; 146uy; 146uy; 145uy; 148uy; 150uy; 153uy; 154uy; 143uy; 98uy; 60uy; 38uy; 9uy; 32uy; 75uy; 89uy; 30uy; 84uy; 31uy; 49uy; 58uy; 74uy; 55uy; 51uy; 71uy; 164uy; 82uy; 129uy; 111uy; 28uy; 76uy; 68uy; 67uy; 79uy; 80uy; 83uy; 89uy; 90uy;
//         205uy; 198uy; 184uy; 183uy; 195uy; 192uy; 194uy; 192uy; 193uy; 188uy; 178uy; 169uy; 167uy; 162uy; 155uy; 146uy; 144uy; 143uy; 145uy; 142uy; 144uy; 130uy; 102uy; 42uy; 9uy; 32uy; 71uy; 78uy; 46uy; 90uy; 21uy; 42uy; 77uy; 123uy; 51uy; 95uy; 59uy; 166uy; 114uy; 82uy; 94uy; 80uy; 97uy; 55uy; 77uy; 82uy; 81uy; 88uy; 92uy; 92uy;
//         216uy; 218uy; 217uy; 212uy; 217uy; 220uy; 222uy; 223uy; 223uy; 213uy; 187uy; 170uy; 171uy; 169uy; 168uy; 164uy; 157uy; 146uy; 142uy; 137uy; 128uy; 122uy; 124uy; 87uy; 19uy; 24uy; 69uy; 70uy; 46uy; 86uy; 19uy; 39uy; 77uy; 115uy; 41uy; 126uy; 47uy; 112uy; 84uy; 74uy; 78uy; 78uy; 75uy; 78uy; 74uy; 85uy; 87uy; 88uy; 94uy; 95uy;
//         223uy; 224uy; 225uy; 227uy; 232uy; 236uy; 239uy; 240uy; 241uy; 240uy; 225uy; 178uy; 172uy; 170uy; 172uy; 167uy; 167uy; 164uy; 158uy; 137uy; 120uy; 115uy; 124uy; 116uy; 90uy; 35uy; 64uy; 68uy; 52uy; 88uy; 36uy; 61uy; 61uy; 94uy; 42uy; 100uy; 45uy; 83uy; 66uy; 69uy; 79uy; 62uy; 77uy; 83uy; 81uy; 88uy; 91uy; 90uy; 94uy; 97uy;
//         226uy; 225uy; 226uy; 227uy; 230uy; 235uy; 237uy; 240uy; 241uy; 241uy; 241uy; 220uy; 175uy; 173uy; 172uy; 170uy; 170uy; 168uy; 168uy; 163uy; 140uy; 109uy; 118uy; 119uy; 115uy; 92uy; 62uy; 59uy; 51uy; 75uy; 68uy; 87uy; 41uy; 96uy; 35uy; 76uy; 50uy; 87uy; 57uy; 61uy; 78uy; 73uy; 80uy; 85uy; 86uy; 90uy; 92uy; 93uy; 96uy; 98uy;
//         228uy; 226uy; 225uy; 226uy; 228uy; 230uy; 234uy; 237uy; 239uy; 240uy; 241uy; 240uy; 219uy; 176uy; 173uy; 172uy; 173uy; 170uy; 169uy; 167uy; 167uy; 145uy; 110uy; 114uy; 120uy; 112uy; 101uy; 52uy; 51uy; 71uy; 48uy; 69uy; 40uy; 80uy; 32uy; 80uy; 47uy; 72uy; 64uy; 68uy; 77uy; 81uy; 85uy; 88uy; 92uy; 92uy; 95uy; 95uy; 97uy; 98uy;
//         229uy; 228uy; 227uy; 226uy; 227uy; 227uy; 230uy; 232uy; 236uy; 238uy; 241uy; 241uy; 240uy; 218uy; 176uy; 171uy; 172uy; 172uy; 173uy; 170uy; 168uy; 163uy; 151uy; 112uy; 114uy; 116uy; 113uy; 95uy; 63uy; 52uy; 55uy; 68uy; 34uy; 71uy; 35uy; 61uy; 49uy; 68uy; 75uy; 75uy; 82uy; 85uy; 90uy; 91uy; 93uy; 94uy; 97uy; 96uy; 100uy; 97uy;
//         229uy; 228uy; 227uy; 226uy; 228uy; 228uy; 228uy; 229uy; 231uy; 235uy; 238uy; 240uy; 241uy; 240uy; 220uy; 173uy; 174uy; 171uy; 172uy; 172uy; 170uy; 165uy; 164uy; 154uy; 120uy; 106uy; 117uy; 109uy; 100uy; 58uy; 49uy; 66uy; 34uy; 56uy; 37uy; 71uy; 63uy; 74uy; 78uy; 81uy; 85uy; 88uy; 92uy; 93uy; 95uy; 95uy; 99uy; 98uy; 100uy; 100uy;
//         229uy; 228uy; 227uy; 227uy; 228uy; 229uy; 229uy; 229uy; 229uy; 230uy; 234uy; 238uy; 240uy; 241uy; 240uy; 217uy; 176uy; 171uy; 172uy; 170uy; 173uy; 169uy; 167uy; 162uy; 156uy; 126uy; 106uy; 113uy; 107uy; 88uy; 33uy; 36uy; 48uy; 68uy; 43uy; 79uy; 68uy; 80uy; 82uy; 83uy; 89uy; 89uy; 93uy; 94uy; 98uy; 97uy; 99uy; 98uy; 101uy; 102uy;
//         231uy; 229uy; 228uy; 228uy; 229uy; 229uy; 231uy; 230uy; 230uy; 230uy; 231uy; 233uy; 238uy; 240uy; 241uy; 236uy; 215uy; 189uy; 177uy; 172uy; 172uy; 171uy; 170uy; 168uy; 165uy; 157uy; 137uy; 102uy; 108uy; 101uy; 66uy; 21uy; 47uy; 68uy; 61uy; 76uy; 76uy; 80uy; 84uy; 86uy; 90uy; 91uy; 94uy; 95uy; 99uy; 98uy; 100uy; 99uy; 103uy; 102uy;
//         231uy; 230uy; 229uy; 228uy; 229uy; 229uy; 230uy; 229uy; 230uy; 230uy; 230uy; 229uy; 231uy; 234uy; 238uy; 233uy; 221uy; 217uy; 208uy; 191uy; 180uy; 171uy; 172uy; 169uy; 170uy; 166uy; 159uy; 137uy; 100uy; 98uy; 92uy; 36uy; 53uy; 79uy; 69uy; 79uy; 83uy; 84uy; 89uy; 88uy; 93uy; 93uy; 96uy; 97uy; 98uy; 99uy; 100uy; 100uy; 103uy; 104uy;
//         231uy; 230uy; 229uy; 228uy; 229uy; 230uy; 230uy; 230uy; 230uy; 229uy; 229uy; 229uy; 228uy; 227uy; 225uy; 219uy; 217uy; 216uy; 217uy; 214uy; 209uy; 193uy; 179uy; 172uy; 172uy; 167uy; 167uy; 154uy; 112uy; 86uy; 92uy; 70uy; 62uy; 76uy; 79uy; 81uy; 85uy; 89uy; 91uy; 92uy; 95uy; 95uy; 98uy; 97uy; 100uy; 99uy; 101uy; 102uy; 104uy; 103uy;
//         230uy; 229uy; 228uy; 229uy; 229uy; 229uy; 230uy; 230uy; 230uy; 229uy; 228uy; 228uy; 227uy; 221uy; 199uy; 196uy; 215uy; 214uy; 215uy; 216uy; 215uy; 213uy; 209uy; 194uy; 179uy; 171uy; 164uy; 126uy; 78uy; 67uy; 75uy; 74uy; 75uy; 78uy; 83uy; 85uy; 90uy; 90uy; 94uy; 92uy; 97uy; 96uy; 98uy; 98uy; 100uy; 100uy; 103uy; 102uy; 105uy; 104uy;
//         229uy; 228uy; 229uy; 229uy; 230uy; 229uy; 230uy; 229uy; 230uy; 229uy; 229uy; 228uy; 227uy; 217uy; 185uy; 181uy; 206uy; 213uy; 215uy; 214uy; 214uy; 213uy; 213uy; 210uy; 206uy; 188uy; 144uy; 69uy; 67uy; 66uy; 79uy; 74uy; 80uy; 82uy; 86uy; 89uy; 91uy; 93uy; 95uy; 94uy; 98uy; 97uy; 99uy; 99uy; 101uy; 101uy; 104uy; 102uy; 107uy; 104uy;
//         227uy; 226uy; 227uy; 228uy; 229uy; 229uy; 229uy; 229uy; 230uy; 229uy; 229uy; 228uy; 226uy; 212uy; 177uy; 176uy; 189uy; 208uy; 215uy; 214uy; 215uy; 213uy; 213uy; 211uy; 210uy; 203uy; 134uy; 57uy; 76uy; 70uy; 84uy; 81uy; 85uy; 85uy; 90uy; 91uy; 93uy; 92uy; 96uy; 95uy; 99uy; 97uy; 100uy; 99uy; 101uy; 102uy; 105uy; 104uy; 106uy; 106uy;
//         226uy; 225uy; 226uy; 226uy; 227uy; 228uy; 228uy; 228uy; 229uy; 228uy; 228uy; 227uy; 225uy; 206uy; 168uy; 174uy; 178uy; 193uy; 210uy; 212uy; 215uy; 214uy; 213uy; 211uy; 209uy; 204uy; 157uy; 64uy; 86uy; 73uy; 80uy; 85uy; 88uy; 91uy; 93uy; 92uy; 94uy; 94uy; 97uy; 96uy; 99uy; 98uy; 100uy; 100uy; 102uy; 103uy; 105uy; 104uy; 107uy; 106uy;
//         226uy; 226uy; 226uy; 225uy; 226uy; 226uy; 227uy; 227uy; 227uy; 226uy; 227uy; 225uy; 223uy; 196uy; 164uy; 172uy; 174uy; 180uy; 198uy; 210uy; 214uy; 214uy; 214uy; 211uy; 209uy; 205uy; 190uy; 113uy; 77uy; 88uy; 83uy; 82uy; 90uy; 92uy; 94uy; 93uy; 97uy; 96uy; 98uy; 97uy; 99uy; 97uy; 101uy; 101uy; 104uy; 104uy; 106uy; 106uy; 108uy; 107uy;
//         227uy; 225uy; 226uy; 225uy; 226uy; 226uy; 226uy; 226uy; 226uy; 225uy; 226uy; 223uy; 220uy; 184uy; 163uy; 168uy; 172uy; 173uy; 183uy; 201uy; 214uy; 213uy; 214uy; 210uy; 210uy; 204uy; 200uy; 174uy; 99uy; 88uy; 93uy; 89uy; 87uy; 87uy; 95uy; 96uy; 97uy; 97uy; 100uy; 99uy; 101uy; 100uy; 102uy; 101uy; 104uy; 104uy; 106uy; 106uy; 108uy; 107uy;
//         228uy; 226uy; 228uy; 226uy; 226uy; 225uy; 226uy; 226uy; 226uy; 226uy; 225uy; 222uy; 215uy; 171uy; 160uy; 162uy; 166uy; 168uy; 172uy; 185uy; 205uy; 212uy; 213uy; 210uy; 210uy; 206uy; 202uy; 193uy; 146uy; 92uy; 104uy; 96uy; 94uy; 93uy; 91uy; 92uy; 97uy; 99uy; 102uy; 101uy; 103uy; 101uy; 103uy; 102uy; 105uy; 104uy; 107uy; 105uy; 109uy; 108uy;
//         230uy; 228uy; 228uy; 227uy; 228uy; 227uy; 227uy; 227uy; 227uy; 226uy; 226uy; 224uy; 211uy; 161uy; 159uy; 158uy; 162uy; 162uy; 167uy; 172uy; 190uy; 207uy; 213uy; 211uy; 210uy; 206uy; 203uy; 197uy; 174uy; 99uy; 111uy; 108uy; 106uy; 99uy; 99uy; 95uy; 94uy; 93uy; 99uy; 101uy; 104uy; 103uy; 105uy; 105uy; 105uy; 105uy; 107uy; 106uy; 109uy; 108uy;
//         232uy; 230uy; 231uy; 230uy; 229uy; 229uy; 229uy; 228uy; 227uy; 227uy; 227uy; 224uy; 205uy; 153uy; 156uy; 154uy; 155uy; 158uy; 161uy; 165uy; 175uy; 192uy; 211uy; 210uy; 210uy; 206uy; 203uy; 197uy; 188uy; 117uy; 101uy; 106uy; 109uy; 109uy; 109uy; 103uy; 101uy; 98uy; 97uy; 96uy; 101uy; 104uy; 106uy; 106uy; 108uy; 107uy; 108uy; 108uy; 110uy; 107uy;
//         232uy; 231uy; 232uy; 231uy; 232uy; 231uy; 231uy; 230uy; 230uy; 229uy; 228uy; 224uy; 200uy; 149uy; 156uy; 150uy; 152uy; 152uy; 154uy; 159uy; 165uy; 175uy; 200uy; 207uy; 207uy; 204uy; 204uy; 197uy; 195uy; 145uy; 97uy; 108uy; 100uy; 96uy; 108uy; 106uy; 109uy; 106uy; 104uy; 99uy; 98uy; 96uy; 100uy; 103uy; 108uy; 107uy; 109uy; 109uy; 111uy; 109uy;
//         233uy; 232uy; 233uy; 234uy; 234uy; 233uy; 233uy; 232uy; 232uy; 230uy; 229uy; 225uy; 191uy; 148uy; 157uy; 150uy; 148uy; 149uy; 150uy; 152uy; 158uy; 164uy; 183uy; 200uy; 205uy; 203uy; 203uy; 199uy; 196uy; 170uy; 104uy; 105uy; 97uy; 92uy; 96uy; 54uy; 65uy; 119uy; 118uy; 107uy; 107uy; 103uy; 103uy; 99uy; 100uy; 102uy; 108uy; 109uy; 110uy; 111uy;
//         233uy; 233uy; 234uy; 233uy; 233uy; 234uy; 234uy; 233uy; 233uy; 232uy; 231uy; 227uy; 186uy; 152uy; 159uy; 150uy; 149uy; 146uy; 146uy; 146uy; 152uy; 156uy; 168uy; 184uy; 202uy; 201uy; 200uy; 197uy; 194uy; 181uy; 124uy; 97uy; 100uy; 92uy; 89uy; 35uy; 25uy; 84uy; 95uy; 105uy; 118uy; 119uy; 112uy; 110uy; 107uy; 102uy; 102uy; 102uy; 107uy; 109uy;
//         231uy; 231uy; 232uy; 232uy; 233uy; 234uy; 234uy; 234uy; 234uy; 234uy; 232uy; 227uy; 188uy; 158uy; 160uy; 157uy; 151uy; 147uy; 146uy; 144uy; 148uy; 150uy; 158uy; 167uy; 186uy; 199uy; 198uy; 195uy; 193uy; 186uy; 150uy; 94uy; 104uy; 90uy; 83uy; 41uy; 42uy; 54uy; 61uy; 63uy; 74uy; 105uy; 123uy; 126uy; 118uy; 113uy; 111uy; 107uy; 105uy; 103uy;
//         230uy; 230uy; 230uy; 230uy; 229uy; 231uy; 229uy; 227uy; 224uy; 217uy; 215uy; 210uy; 199uy; 171uy; 164uy; 158uy; 156uy; 150uy; 147uy; 143uy; 146uy; 147uy; 152uy; 157uy; 168uy; 191uy; 195uy; 191uy; 191uy; 186uy; 168uy; 112uy; 105uy; 96uy; 104uy; 143uy; 135uy; 137uy; 140uy; 135uy; 136uy; 136uy; 137uy; 136uy; 140uy; 134uy; 126uy; 119uy; 114uy; 112uy;
//         227uy; 227uy; 228uy; 222uy; 192uy; 207uy; 197uy; 200uy; 200uy; 201uy; 202uy; 200uy; 202uy; 192uy; 172uy; 162uy; 159uy; 154uy; 151uy; 145uy; 147uy; 147uy; 150uy; 150uy; 153uy; 170uy; 189uy; 186uy; 188uy; 182uy; 176uy; 151uy; 128uy; 113uy; 112uy; 136uy; 146uy; 146uy; 146uy; 141uy; 147uy; 143uy; 145uy; 145uy; 142uy; 142uy; 141uy; 138uy; 133uy; 126uy;
//         223uy; 224uy; 225uy; 209uy; 141uy; 201uy; 188uy; 198uy; 197uy; 198uy; 201uy; 201uy; 203uy; 202uy; 193uy; 169uy; 162uy; 155uy; 153uy; 148uy; 147uy; 147uy; 150uy; 148uy; 145uy; 152uy; 173uy; 184uy; 183uy; 178uy; 177uy; 172uy; 168uy; 138uy; 113uy; 103uy; 141uy; 144uy; 139uy; 142uy; 146uy; 141uy; 142uy; 141uy; 143uy; 140uy; 142uy; 140uy; 141uy; 145uy;
//         219uy; 220uy; 222uy; 197uy; 108uy; 192uy; 188uy; 192uy; 192uy; 196uy; 199uy; 200uy; 203uy; 202uy; 202uy; 186uy; 165uy; 156uy; 155uy; 150uy; 150uy; 148uy; 151uy; 148uy; 145uy; 143uy; 151uy; 173uy; 181uy; 175uy; 173uy; 172uy; 176uy; 164uy; 109uy; 83uy; 113uy; 148uy; 136uy; 150uy; 153uy; 145uy; 143uy; 141uy; 141uy; 141uy; 142uy; 142uy; 143uy; 143uy;
//         215uy; 217uy; 219uy; 177uy; 93uy; 160uy; 197uy; 181uy; 194uy; 195uy; 200uy; 203uy; 205uy; 202uy; 203uy; 197uy; 181uy; 157uy; 153uy; 150uy; 149uy; 147uy; 152uy; 147uy; 143uy; 140uy; 131uy; 149uy; 182uy; 172uy; 172uy; 173uy; 175uy; 170uy; 111uy; 72uy; 98uy; 138uy; 146uy; 159uy; 149uy; 141uy; 145uy; 139uy; 141uy; 143uy; 149uy; 145uy; 140uy; 140uy;
//         214uy; 214uy; 215uy; 158uy; 91uy; 127uy; 194uy; 180uy; 195uy; 195uy; 204uy; 204uy; 205uy; 202uy; 201uy; 198uy; 192uy; 172uy; 153uy; 147uy; 148uy; 146uy; 148uy; 147uy; 144uy; 134uy; 114uy; 129uy; 191uy; 165uy; 174uy; 173uy; 174uy; 170uy; 114uy; 65uy; 92uy; 118uy; 162uy; 130uy; 147uy; 136uy; 141uy; 138uy; 135uy; 134uy; 149uy; 139uy; 127uy; 137uy;
//         214uy; 214uy; 213uy; 141uy; 98uy; 106uy; 171uy; 197uy; 199uy; 208uy; 217uy; 217uy; 212uy; 206uy; 202uy; 197uy; 191uy; 183uy; 166uy; 146uy; 146uy; 145uy; 147uy; 144uy; 143uy; 129uy; 98uy; 121uy; 197uy; 165uy; 176uy; 172uy; 171uy; 170uy; 116uy; 61uy; 88uy; 99uy; 107uy; 65uy; 174uy; 126uy; 145uy; 131uy; 62uy; 60uy; 156uy; 126uy; 46uy; 110uy;
//         214uy; 214uy; 212uy; 126uy; 103uy; 98uy; 145uy; 219uy; 224uy; 231uy; 234uy; 236uy; 225uy; 214uy; 203uy; 192uy; 189uy; 185uy; 180uy; 156uy; 143uy; 143uy; 143uy; 143uy; 141uy; 119uy; 85uy; 119uy; 199uy; 167uy; 180uy; 172uy; 172uy; 168uy; 118uy; 58uy; 85uy; 86uy; 97uy; 75uy; 177uy; 123uy; 146uy; 79uy; 47uy; 137uy; 141uy; 122uy; 56uy; 31uy;
//         214uy; 213uy; 207uy; 116uy; 108uy; 92uy; 138uy; 224uy; 225uy; 228uy; 227uy; 228uy; 224uy; 209uy; 166uy; 171uy; 189uy; 183uy; 183uy; 173uy; 154uy; 141uy; 142uy; 140uy; 137uy; 105uy; 77uy; 113uy; 200uy; 168uy; 181uy; 172uy; 171uy; 167uy; 122uy; 53uy; 83uy; 78uy; 110uy; 101uy; 168uy; 124uy; 130uy; 76uy; 111uy; 158uy; 131uy; 74uy; 41uy; 133uy;
//         216uy; 215uy; 205uy; 108uy; 110uy; 90uy; 119uy; 213uy; 227uy; 224uy; 229uy; 231uy; 228uy; 191uy; 151uy; 145uy; 185uy; 179uy; 184uy; 177uy; 173uy; 152uy; 139uy; 136uy; 131uy; 90uy; 79uy; 107uy; 200uy; 170uy; 181uy; 170uy; 172uy; 169uy; 126uy; 52uy; 80uy; 73uy; 94uy; 98uy; 140uy; 113uy; 156uy; 121uy; 82uy; 156uy; 130uy; 105uy; 68uy; 88uy;
//         216uy; 216uy; 204uy; 101uy; 112uy; 92uy; 114uy; 200uy; 231uy; 224uy; 233uy; 234uy; 226uy; 157uy; 163uy; 139uy; 167uy; 181uy; 180uy; 178uy; 180uy; 169uy; 152uy; 134uy; 121uy; 78uy; 84uy; 97uy; 196uy; 176uy; 180uy; 169uy; 173uy; 170uy; 134uy; 54uy; 76uy; 72uy; 70uy; 75uy; 86uy; 37uy; 154uy; 134uy; 67uy; 133uy; 144uy; 135uy; 83uy; 72uy;
//         218uy; 217uy; 201uy; 97uy; 115uy; 99uy; 108uy; 183uy; 231uy; 222uy; 233uy; 233uy; 213uy; 137uy; 163uy; 164uy; 141uy; 185uy; 178uy; 179uy; 181uy; 178uy; 173uy; 144uy; 105uy; 76uy; 87uy; 92uy; 188uy; 181uy; 179uy; 170uy; 174uy; 171uy; 140uy; 55uy; 74uy; 69uy; 67uy; 74uy; 139uy; 48uy; 57uy; 125uy; 77uy; 96uy; 163uy; 114uy; 99uy; 124uy;
//         220uy; 218uy; 197uy; 92uy; 120uy; 106uy; 107uy; 168uy; 230uy; 225uy; 232uy; 231uy; 190uy; 132uy; 156uy; 175uy; 141uy; 171uy; 187uy; 181uy; 185uy; 183uy; 180uy; 160uy; 92uy; 77uy; 87uy; 93uy; 186uy; 182uy; 179uy; 172uy; 175uy; 173uy; 145uy; 55uy; 75uy; 66uy; 68uy; 85uy; 163uy; 124uy; 124uy; 79uy; 55uy; 77uy; 171uy; 61uy; 87uy; 128uy;
//         221uy; 219uy; 192uy; 89uy; 127uy; 112uy; 119uy; 157uy; 228uy; 227uy; 232uy; 229uy; 167uy; 140uy; 147uy; 165uy; 168uy; 158uy; 196uy; 182uy; 190uy; 185uy; 182uy; 168uy; 97uy; 77uy; 91uy; 94uy; 176uy; 184uy; 179uy; 175uy; 179uy; 173uy; 145uy; 54uy; 75uy; 65uy; 67uy; 83uy; 167uy; 130uy; 140uy; 131uy; 72uy; 56uy; 170uy; 72uy; 18uy; 51uy;
//         222uy; 220uy; 187uy; 88uy; 131uy; 113uy; 124uy; 156uy; 222uy; 230uy; 232uy; 222uy; 155uy; 149uy; 147uy; 157uy; 175uy; 169uy; 191uy; 192uy; 190uy; 184uy; 184uy; 173uy; 107uy; 75uy; 97uy; 94uy; 165uy; 187uy; 176uy; 176uy; 182uy; 175uy; 146uy; 58uy; 74uy; 66uy; 64uy; 37uy; 64uy; 54uy; 61uy; 78uy; 107uy; 133uy; 140uy; 125uy; 66uy; 122uy;
//         222uy; 220uy; 178uy; 89uy; 131uy; 114uy; 121uy; 162uy; 215uy; 211uy; 229uy; 212uy; 149uy; 160uy; 153uy; 160uy; 173uy; 184uy; 186uy; 198uy; 193uy; 187uy; 185uy; 178uy; 116uy; 77uy; 102uy; 101uy; 170uy; 189uy; 173uy; 172uy; 177uy; 172uy; 140uy; 64uy; 74uy; 67uy; 74uy; 93uy; 94uy; 97uy; 93uy; 86uy; 76uy; 108uy; 147uy; 131uy; 108uy; 120uy;
//         222uy; 220uy; 162uy; 91uy; 132uy; 115uy; 122uy; 173uy; 209uy; 193uy; 207uy; 198uy; 164uy; 172uy; 172uy; 176uy; 183uy; 187uy; 192uy; 197uy; 201uy; 193uy; 187uy; 182uy; 124uy; 77uy; 107uy; 108uy; 184uy; 192uy; 180uy; 176uy; 177uy; 166uy; 128uy; 70uy; 76uy; 67uy; 82uy; 154uy; 139uy; 146uy; 142uy; 135uy; 135uy; 128uy; 134uy; 128uy; 122uy; 113uy;
//         224uy; 220uy; 143uy; 96uy; 130uy; 118uy; 128uy; 192uy; 200uy; 193uy; 194uy; 188uy; 179uy; 181uy; 181uy; 179uy; 180uy; 181uy; 187uy; 188uy; 193uy; 192uy; 193uy; 192uy; 133uy; 74uy; 111uy; 118uy; 199uy; 192uy; 189uy; 175uy; 172uy; 166uy; 122uy; 73uy; 79uy; 68uy; 85uy; 160uy; 142uy; 146uy; 145uy; 139uy; 139uy; 131uy; 126uy; 112uy; 103uy; 92uy;
//         230uy; 220uy; 129uy; 103uy; 127uy; 119uy; 140uy; 205uy; 190uy; 195uy; 192uy; 189uy; 186uy; 182uy; 181uy; 179uy; 179uy; 175uy; 181uy; 181uy; 182uy; 179uy; 179uy; 171uy; 117uy; 80uy; 109uy; 128uy; 206uy; 191uy; 195uy; 182uy; 172uy; 165uy; 117uy; 73uy; 80uy; 69uy; 90uy; 167uy; 139uy; 147uy; 146uy; 144uy; 137uy; 123uy; 108uy; 90uy; 78uy; 65uy;
//         225uy; 202uy; 118uy; 110uy; 123uy; 121uy; 157uy; 218uy; 190uy; 194uy; 192uy; 191uy; 193uy; 188uy; 189uy; 185uy; 182uy; 177uy; 178uy; 178uy; 177uy; 171uy; 168uy; 138uy; 80uy; 91uy; 98uy; 131uy; 208uy; 186uy; 197uy; 188uy; 181uy; 162uy; 113uy; 75uy; 82uy; 70uy; 94uy; 173uy; 133uy; 157uy; 147uy; 147uy; 134uy; 109uy; 87uy; 72uy; 61uy; 59uy;
//         218uy; 160uy; 116uy; 113uy; 124uy; 119uy; 164uy; 227uy; 200uy; 198uy; 187uy; 190uy; 193uy; 191uy; 189uy; 186uy; 183uy; 178uy; 176uy; 173uy; 168uy; 157uy; 145uy; 103uy; 73uy; 93uy; 95uy; 123uy; 207uy; 183uy; 195uy; 186uy; 185uy; 170uy; 113uy; 77uy; 82uy; 71uy; 96uy; 169uy; 131uy; 156uy; 147uy; 143uy; 118uy; 90uy; 74uy; 59uy; 57uy; 56uy;
//         188uy; 128uy; 128uy; 115uy; 125uy; 121uy; 167uy; 228uy; 222uy; 218uy; 193uy; 189uy; 193uy; 190uy; 188uy; 184uy; 179uy; 171uy; 168uy; 159uy; 152uy; 143uy; 128uy; 83uy; 77uy; 87uy; 91uy; 112uy; 200uy; 183uy; 191uy; 184uy; 184uy; 174uy; 112uy; 74uy; 82uy; 74uy; 96uy; 165uy; 128uy; 142uy; 129uy; 121uy; 90uy; 69uy; 56uy; 53uy; 54uy; 52uy;
//         150uy; 128uy; 130uy; 121uy; 125uy; 124uy; 191uy; 232uy; 225uy; 230uy; 219uy; 193uy; 186uy; 180uy; 176uy; 169uy; 166uy; 161uy; 161uy; 155uy; 152uy; 143uy; 117uy; 76uy; 82uy; 83uy; 87uy; 105uy; 192uy; 182uy; 187uy; 178uy; 181uy; 168uy; 102uy; 76uy; 79uy; 74uy; 79uy; 102uy; 90uy; 79uy; 55uy; 42uy; 36uy; 40uy; 42uy; 37uy; 37uy; 35uy;
//         147uy; 139uy; 136uy; 128uy; 124uy; 146uy; 220uy; 230uy; 229uy; 230uy; 228uy; 190uy; 170uy; 171uy; 167uy; 164uy; 164uy; 162uy; 162uy; 158uy; 155uy; 143uy; 106uy; 76uy; 85uy; 81uy; 84uy; 102uy; 190uy; 177uy; 182uy; 174uy; 177uy; 159uy; 90uy; 83uy; 79uy; 71uy; 52uy; 51uy; 48uy; 42uy; 36uy; 31uy; 33uy; 37uy; 36uy; 32uy; 33uy; 32uy;
// |]

// let rightInput = [|
//         150uy; 149uy; 153uy; 156uy; 161uy; 159uy; 160uy; 158uy; 152uy; 115uy; 44uy; 34uy; 34uy; 32uy; 35uy; 36uy; 40uy; 38uy; 64uy; 130uy; 8uy; 65uy; 46uy; 59uy; 71uy; 66uy; 77uy; 72uy; 74uy; 140uy; 102uy; 83uy; 107uy; 63uy; 89uy; 87uy; 91uy; 94uy; 96uy; 97uy; 100uy; 100uy; 102uy; 102uy; 105uy; 105uy; 107uy; 107uy; 110uy; 108uy;
//         168uy; 157uy; 149uy; 145uy; 147uy; 146uy; 150uy; 150uy; 155uy; 153uy; 138uy; 77uy; 26uy; 34uy; 35uy; 36uy; 48uy; 44uy; 59uy; 106uy; 23uy; 89uy; 30uy; 51uy; 57uy; 74uy; 62uy; 50uy; 65uy; 154uy; 69uy; 75uy; 80uy; 85uy; 87uy; 90uy; 94uy; 94uy; 98uy; 98uy; 101uy; 100uy; 103uy; 102uy; 105uy; 105uy; 108uy; 107uy; 109uy; 108uy;
//         173uy; 169uy; 166uy; 159uy; 151uy; 143uy; 144uy; 142uy; 143uy; 143uy; 142uy; 126uy; 80uy; 27uy; 39uy; 37uy; 50uy; 43uy; 55uy; 90uy; 34uy; 102uy; 19uy; 42uy; 67uy; 121uy; 55uy; 88uy; 64uy; 96uy; 73uy; 79uy; 84uy; 87uy; 90uy; 92uy; 95uy; 96uy; 100uy; 100uy; 101uy; 101uy; 104uy; 104uy; 106uy; 106uy; 108uy; 108uy; 108uy; 109uy;
//         176uy; 171uy; 169uy; 166uy; 167uy; 160uy; 152uy; 143uy; 140uy; 135uy; 126uy; 124uy; 120uy; 80uy; 35uy; 39uy; 47uy; 40uy; 53uy; 85uy; 36uy; 100uy; 16uy; 40uy; 64uy; 116uy; 43uy; 108uy; 62uy; 78uy; 79uy; 83uy; 89uy; 89uy; 94uy; 95uy; 97uy; 97uy; 99uy; 99uy; 102uy; 103uy; 106uy; 105uy; 108uy; 106uy; 109uy; 109uy; 111uy; 111uy;
//         204uy; 172uy; 171uy; 169uy; 171uy; 167uy; 167uy; 162uy; 151uy; 129uy; 119uy; 116uy; 124uy; 114uy; 87uy; 36uy; 40uy; 33uy; 51uy; 80uy; 37uy; 100uy; 25uy; 64uy; 52uy; 103uy; 47uy; 80uy; 73uy; 80uy; 83uy; 87uy; 91uy; 90uy; 95uy; 95uy; 99uy; 98uy; 100uy; 101uy; 104uy; 103uy; 106uy; 105uy; 108uy; 106uy; 109uy; 110uy; 111uy; 113uy;
//         237uy; 199uy; 172uy; 171uy; 173uy; 169uy; 169uy; 167uy; 168uy; 158uy; 127uy; 110uy; 118uy; 119uy; 114uy; 88uy; 39uy; 31uy; 50uy; 73uy; 36uy; 91uy; 50uy; 108uy; 29uy; 100uy; 60uy; 78uy; 79uy; 83uy; 88uy; 89uy; 93uy; 93uy; 96uy; 97uy; 100uy; 100uy; 102uy; 101uy; 104uy; 105uy; 107uy; 105uy; 108uy; 107uy; 109uy; 110uy; 112uy; 111uy;
//         242uy; 235uy; 194uy; 169uy; 174uy; 171uy; 171uy; 169uy; 168uy; 167uy; 161uy; 129uy; 104uy; 120uy; 115uy; 111uy; 91uy; 37uy; 46uy; 72uy; 35uy; 82uy; 37uy; 73uy; 46uy; 74uy; 76uy; 80uy; 85uy; 88uy; 91uy; 91uy; 95uy; 95uy; 97uy; 98uy; 101uy; 101uy; 102uy; 103uy; 105uy; 105uy; 106uy; 106uy; 109uy; 108uy; 111uy; 110uy; 113uy; 111uy;
//         242uy; 242uy; 235uy; 193uy; 170uy; 172uy; 173uy; 170uy; 171uy; 167uy; 167uy; 160uy; 138uy; 104uy; 119uy; 113uy; 111uy; 91uy; 48uy; 56uy; 32uy; 71uy; 45uy; 78uy; 67uy; 77uy; 81uy; 83uy; 89uy; 90uy; 92uy; 94uy; 96uy; 97uy; 99uy; 99uy; 102uy; 101uy; 104uy; 105uy; 107uy; 106uy; 108uy; 108uy; 110uy; 109uy; 111uy; 110uy; 114uy; 113uy;
//         240uy; 241uy; 241uy; 234uy; 194uy; 169uy; 172uy; 171uy; 172uy; 170uy; 168uy; 164uy; 162uy; 141uy; 108uy; 112uy; 113uy; 108uy; 96uy; 45uy; 45uy; 84uy; 46uy; 80uy; 76uy; 81uy; 85uy; 87uy; 92uy; 91uy; 95uy; 96uy; 98uy; 98uy; 99uy; 101uy; 103uy; 103uy; 105uy; 105uy; 107uy; 106uy; 109uy; 109uy; 111uy; 109uy; 111uy; 111uy; 113uy; 113uy;
//         237uy; 239uy; 240uy; 241uy; 235uy; 192uy; 172uy; 171uy; 171uy; 170uy; 172uy; 168uy; 165uy; 160uy; 149uy; 112uy; 107uy; 112uy; 106uy; 80uy; 43uy; 75uy; 64uy; 80uy; 82uy; 83uy; 89uy; 89uy; 92uy; 94uy; 96uy; 97uy; 99uy; 98uy; 101uy; 100uy; 104uy; 103uy; 106uy; 104uy; 107uy; 107uy; 110uy; 110uy; 112uy; 111uy; 113uy; 112uy; 114uy; 113uy;
//         231uy; 234uy; 238uy; 241uy; 240uy; 228uy; 202uy; 182uy; 174uy; 172uy; 171uy; 169uy; 170uy; 166uy; 162uy; 153uy; 125uy; 99uy; 107uy; 93uy; 69uy; 70uy; 78uy; 80uy; 84uy; 86uy; 91uy; 91uy; 94uy; 95uy; 98uy; 98uy; 100uy; 100uy; 102uy; 103uy; 105uy; 104uy; 106uy; 105uy; 107uy; 107uy; 110uy; 110uy; 112uy; 111uy; 115uy; 114uy; 113uy; 113uy;
//         229uy; 229uy; 233uy; 236uy; 237uy; 225uy; 219uy; 212uy; 198uy; 184uy; 175uy; 170uy; 171uy; 167uy; 167uy; 162uy; 153uy; 121uy; 88uy; 77uy; 70uy; 77uy; 81uy; 83uy; 89uy; 90uy; 93uy; 93uy; 97uy; 96uy; 98uy; 98uy; 101uy; 101uy; 103uy; 103uy; 106uy; 106uy; 107uy; 108uy; 109uy; 109uy; 110uy; 110uy; 112uy; 112uy; 118uy; 119uy; 113uy; 115uy;
//         228uy; 228uy; 228uy; 227uy; 224uy; 217uy; 216uy; 215uy; 215uy; 211uy; 200uy; 182uy; 173uy; 169uy; 166uy; 160uy; 152uy; 121uy; 71uy; 73uy; 78uy; 83uy; 85uy; 87uy; 91uy; 91uy; 96uy; 95uy; 98uy; 98uy; 100uy; 99uy; 102uy; 102uy; 104uy; 104uy; 107uy; 106uy; 109uy; 108uy; 110uy; 109uy; 111uy; 110uy; 112uy; 111uy; 114uy; 113uy; 114uy; 114uy;
//         228uy; 227uy; 226uy; 213uy; 192uy; 208uy; 215uy; 215uy; 215uy; 213uy; 214uy; 209uy; 198uy; 158uy; 114uy; 95uy; 91uy; 80uy; 71uy; 78uy; 82uy; 84uy; 89uy; 91uy; 94uy; 93uy; 95uy; 96uy; 98uy; 99uy; 100uy; 100uy; 103uy; 103uy; 105uy; 104uy; 108uy; 109uy; 110uy; 109uy; 111uy; 110uy; 112uy; 112uy; 114uy; 112uy; 113uy; 112uy; 115uy; 114uy;
//         229uy; 227uy; 224uy; 203uy; 175uy; 191uy; 213uy; 213uy; 215uy; 213uy; 214uy; 212uy; 194uy; 91uy; 68uy; 68uy; 83uy; 75uy; 81uy; 81uy; 86uy; 89uy; 92uy; 92uy; 96uy; 95uy; 97uy; 97uy; 100uy; 99uy; 101uy; 101uy; 103uy; 103uy; 106uy; 105uy; 109uy; 108uy; 109uy; 109uy; 111uy; 110uy; 114uy; 112uy; 114uy; 112uy; 115uy; 113uy; 116uy; 116uy;
//         228uy; 227uy; 223uy; 197uy; 171uy; 180uy; 198uy; 212uy; 214uy; 214uy; 214uy; 212uy; 169uy; 60uy; 73uy; 67uy; 84uy; 80uy; 83uy; 86uy; 91uy; 90uy; 94uy; 94uy; 95uy; 95uy; 98uy; 97uy; 100uy; 101uy; 103uy; 102uy; 105uy; 103uy; 107uy; 106uy; 109uy; 109uy; 110uy; 109uy; 112uy; 111uy; 114uy; 113uy; 115uy; 113uy; 114uy; 115uy; 116uy; 116uy;
//         227uy; 227uy; 220uy; 183uy; 169uy; 175uy; 185uy; 202uy; 214uy; 212uy; 214uy; 213uy; 173uy; 65uy; 85uy; 71uy; 80uy; 85uy; 88uy; 90uy; 93uy; 91uy; 95uy; 95uy; 96uy; 97uy; 98uy; 98uy; 100uy; 101uy; 103uy; 103uy; 105uy; 105uy; 107uy; 106uy; 110uy; 110uy; 110uy; 111uy; 113uy; 113uy; 114uy; 114uy; 114uy; 112uy; 114uy; 114uy; 114uy; 115uy;
//         226uy; 224uy; 215uy; 170uy; 168uy; 172uy; 178uy; 186uy; 206uy; 213uy; 214uy; 212uy; 199uy; 110uy; 80uy; 88uy; 83uy; 81uy; 90uy; 92uy; 95uy; 94uy; 97uy; 98uy; 99uy; 98uy; 99uy; 100uy; 100uy; 101uy; 103uy; 104uy; 107uy; 106uy; 107uy; 108uy; 110uy; 111uy; 111uy; 111uy; 112uy; 113uy; 115uy; 114uy; 115uy; 114uy; 116uy; 114uy; 116uy; 114uy;
//         224uy; 222uy; 208uy; 165uy; 167uy; 168uy; 173uy; 175uy; 191uy; 208uy; 214uy; 213uy; 211uy; 184uy; 116uy; 92uy; 95uy; 90uy; 86uy; 89uy; 96uy; 97uy; 99uy; 97uy; 100uy; 99uy; 101uy; 98uy; 102uy; 101uy; 105uy; 104uy; 107uy; 106uy; 108uy; 108uy; 109uy; 109uy; 113uy; 110uy; 114uy; 113uy; 114uy; 114uy; 115uy; 114uy; 115uy; 113uy; 116uy; 114uy;
//         223uy; 220uy; 199uy; 157uy; 166uy; 162uy; 168uy; 169uy; 177uy; 192uy; 209uy; 212uy; 210uy; 205uy; 193uy; 142uy; 107uy; 99uy; 97uy; 93uy; 91uy; 93uy; 98uy; 98uy; 101uy; 100uy; 102uy; 102uy; 103uy; 102uy; 103uy; 104uy; 108uy; 105uy; 108uy; 108uy; 111uy; 110uy; 112uy; 111uy; 115uy; 113uy; 114uy; 113uy; 115uy; 114uy; 116uy; 115uy; 117uy; 114uy;
//         223uy; 219uy; 188uy; 151uy; 163uy; 156uy; 162uy; 164uy; 169uy; 178uy; 198uy; 210uy; 212uy; 208uy; 205uy; 196uy; 172uy; 133uy; 108uy; 98uy; 99uy; 94uy; 93uy; 95uy; 101uy; 101uy; 105uy; 103uy; 105uy; 104uy; 106uy; 105uy; 107uy; 105uy; 109uy; 108uy; 111uy; 110uy; 112uy; 112uy; 114uy; 113uy; 115uy; 113uy; 115uy; 114uy; 115uy; 115uy; 116uy; 113uy;
//         225uy; 218uy; 180uy; 147uy; 158uy; 153uy; 156uy; 159uy; 163uy; 166uy; 181uy; 201uy; 211uy; 208uy; 208uy; 201uy; 197uy; 184uy; 154uy; 111uy; 111uy; 100uy; 102uy; 97uy; 95uy; 97uy; 102uy; 105uy; 106uy; 105uy; 108uy; 106uy; 109uy; 109uy; 108uy; 109uy; 110uy; 111uy; 113uy; 112uy; 114uy; 112uy; 114uy; 113uy; 115uy; 113uy; 115uy; 114uy; 114uy; 113uy;
//         226uy; 215uy; 166uy; 149uy; 153uy; 150uy; 152uy; 153uy; 157uy; 160uy; 169uy; 186uy; 206uy; 207uy; 206uy; 203uy; 201uy; 195uy; 181uy; 117uy; 115uy; 117uy; 110uy; 104uy; 103uy; 98uy; 98uy; 96uy; 102uy; 105uy; 108uy; 108uy; 110uy; 108uy; 111uy; 111uy; 111uy; 110uy; 113uy; 113uy; 115uy; 114uy; 114uy; 113uy; 115uy; 115uy; 116uy; 115uy; 115uy; 115uy;
//         228uy; 213uy; 158uy; 153uy; 153uy; 148uy; 149uy; 147uy; 151uy; 155uy; 160uy; 170uy; 193uy; 204uy; 203uy; 202uy; 202uy; 196uy; 192uy; 144uy; 98uy; 109uy; 112uy; 115uy; 113uy; 106uy; 106uy; 102uy; 99uy; 99uy; 102uy; 105uy; 109uy; 109uy; 111uy; 110uy; 112uy; 112uy; 113uy; 114uy; 115uy; 113uy; 116uy; 114uy; 116uy; 114uy; 117uy; 113uy; 116uy; 113uy;
//         230uy; 214uy; 158uy; 156uy; 154uy; 150uy; 149uy; 146uy; 147uy; 149uy; 153uy; 160uy; 172uy; 192uy; 202uy; 200uy; 200uy; 196uy; 192uy; 168uy; 107uy; 102uy; 99uy; 97uy; 101uy; 108uy; 124uy; 113uy; 112uy; 108uy; 105uy; 102uy; 101uy; 103uy; 110uy; 110uy; 113uy; 113uy; 114uy; 114uy; 115uy; 113uy; 116uy; 114uy; 116uy; 115uy; 115uy; 114uy; 115uy; 112uy;
//         230uy; 212uy; 164uy; 158uy; 158uy; 152uy; 150uy; 145uy; 145uy; 145uy; 150uy; 151uy; 162uy; 173uy; 195uy; 199uy; 197uy; 194uy; 190uy; 180uy; 129uy; 95uy; 101uy; 93uy; 82uy; 70uy; 100uy; 108uy; 127uy; 120uy; 114uy; 110uy; 109uy; 105uy; 103uy; 103uy; 109uy; 110uy; 114uy; 113uy; 115uy; 114uy; 115uy; 114uy; 116uy; 115uy; 116uy; 114uy; 115uy; 114uy;
//         214uy; 207uy; 184uy; 162uy; 163uy; 156uy; 153uy; 148uy; 145uy; 144uy; 146uy; 148uy; 155uy; 158uy; 176uy; 195uy; 193uy; 191uy; 190uy; 183uy; 155uy; 100uy; 106uy; 92uy; 102uy; 144uy; 129uy; 136uy; 138uy; 136uy; 140uy; 129uy; 121uy; 116uy; 113uy; 110uy; 107uy; 104uy; 106uy; 108uy; 113uy; 112uy; 115uy; 114uy; 116uy; 115uy; 116uy; 114uy; 114uy; 113uy;
//         202uy; 201uy; 200uy; 177uy; 165uy; 160uy; 156uy; 151uy; 148uy; 143uy; 148uy; 145uy; 151uy; 150uy; 158uy; 178uy; 190uy; 186uy; 187uy; 180uy; 172uy; 141uy; 121uy; 111uy; 112uy; 147uy; 146uy; 147uy; 144uy; 146uy; 147uy; 137uy; 142uy; 134uy; 129uy; 122uy; 118uy; 116uy; 114uy; 112uy; 107uy; 107uy; 113uy; 113uy; 115uy; 113uy; 115uy; 114uy; 115uy; 114uy;
//         201uy; 202uy; 204uy; 198uy; 178uy; 162uy; 159uy; 154uy; 150uy; 147uy; 147uy; 148uy; 151uy; 146uy; 147uy; 158uy; 180uy; 182uy; 183uy; 177uy; 175uy; 171uy; 160uy; 127uy; 109uy; 104uy; 149uy; 147uy; 148uy; 139uy; 142uy; 141uy; 146uy; 139uy; 142uy; 143uy; 140uy; 131uy; 128uy; 136uy; 129uy; 125uy; 124uy; 113uy; 108uy; 108uy; 113uy; 112uy; 113uy; 115uy;
//         201uy; 201uy; 204uy; 203uy; 198uy; 172uy; 160uy; 155uy; 153uy; 148uy; 148uy; 148uy; 151uy; 145uy; 143uy; 144uy; 159uy; 179uy; 179uy; 173uy; 172uy; 173uy; 175uy; 154uy; 96uy; 88uy; 122uy; 153uy; 133uy; 140uy; 140uy; 145uy; 155uy; 146uy; 143uy; 141uy; 140uy; 142uy; 145uy; 144uy; 139uy; 151uy; 202uy; 181uy; 113uy; 114uy; 105uy; 104uy; 109uy; 110uy;
//         201uy; 203uy; 204uy; 202uy; 201uy; 191uy; 167uy; 153uy; 152uy; 149uy; 148uy; 148uy; 151uy; 145uy; 142uy; 138uy; 132uy; 166uy; 179uy; 171uy; 170uy; 174uy; 176uy; 162uy; 92uy; 80uy; 100uy; 142uy; 138uy; 143uy; 142uy; 157uy; 154uy; 140uy; 143uy; 138uy; 140uy; 142uy; 147uy; 145uy; 145uy; 149uy; 160uy; 157uy; 145uy; 132uy; 125uy; 115uy; 108uy; 102uy;
//         203uy; 204uy; 204uy; 200uy; 201uy; 194uy; 184uy; 157uy; 150uy; 148uy; 148uy; 147uy; 150uy; 145uy; 143uy; 130uy; 106uy; 157uy; 183uy; 168uy; 172uy; 174uy; 172uy; 164uy; 91uy; 73uy; 91uy; 112uy; 146uy; 140uy; 151uy; 137uy; 145uy; 139uy; 141uy; 137uy; 137uy; 136uy; 149uy; 142uy; 129uy; 136uy; 136uy; 127uy; 153uy; 141uy; 141uy; 132uy; 125uy; 116uy;
//         217uy; 213uy; 208uy; 203uy; 199uy; 193uy; 187uy; 175uy; 153uy; 143uy; 146uy; 145uy; 147uy; 144uy; 139uy; 119uy; 90uy; 160uy; 185uy; 171uy; 173uy; 173uy; 173uy; 161uy; 90uy; 68uy; 87uy; 98uy; 150uy; 140uy; 108uy; 59uy; 170uy; 129uy; 144uy; 133uy; 75uy; 51uy; 152uy; 134uy; 52uy; 105uy; 99uy; 74uy; 98uy; 131uy; 154uy; 151uy; 143uy; 135uy;
//         238uy; 231uy; 220uy; 209uy; 196uy; 188uy; 187uy; 182uy; 171uy; 145uy; 144uy; 142uy; 144uy; 143uy; 136uy; 107uy; 84uy; 154uy; 192uy; 170uy; 175uy; 174uy; 171uy; 163uy; 92uy; 63uy; 84uy; 88uy; 150uy; 158uy; 105uy; 58uy; 178uy; 125uy; 144uy; 91uy; 36uy; 130uy; 144uy; 127uy; 61uy; 27uy; 93uy; 90uy; 101uy; 125uy; 165uy; 146uy; 144uy; 120uy;
//         229uy; 224uy; 219uy; 192uy; 158uy; 186uy; 184uy; 183uy; 181uy; 165uy; 146uy; 139uy; 142uy; 139uy; 132uy; 91uy; 81uy; 143uy; 200uy; 167uy; 177uy; 173uy; 169uy; 164uy; 98uy; 59uy; 82uy; 80uy; 131uy; 170uy; 127uy; 85uy; 169uy; 126uy; 137uy; 74uy; 105uy; 154uy; 136uy; 91uy; 33uy; 123uy; 83uy; 37uy; 50uy; 57uy; 69uy; 99uy; 157uy; 107uy;
//         229uy; 229uy; 220uy; 168uy; 140uy; 167uy; 185uy; 182uy; 180uy; 175uy; 164uy; 142uy; 139uy; 135uy; 123uy; 77uy; 84uy; 129uy; 206uy; 166uy; 179uy; 172uy; 169uy; 166uy; 108uy; 55uy; 80uy; 75uy; 108uy; 169uy; 127uy; 91uy; 144uy; 108uy; 157uy; 124uy; 89uy; 141uy; 140uy; 110uy; 62uy; 93uy; 45uy; 67uy; 87uy; 84uy; 113uy; 116uy; 139uy; 28uy;
//         234uy; 233uy; 200uy; 144uy; 163uy; 135uy; 187uy; 177uy; 180uy; 177uy; 175uy; 162uy; 141uy; 130uy; 104uy; 73uy; 87uy; 122uy; 206uy; 169uy; 180uy; 171uy; 171uy; 168uy; 112uy; 52uy; 79uy; 71uy; 95uy; 148uy; 46uy; 77uy; 92uy; 30uy; 151uy; 138uy; 77uy; 119uy; 153uy; 133uy; 95uy; 64uy; 111uy; 91uy; 127uy; 109uy; 157uy; 139uy; 135uy; 99uy;
//         233uy; 231uy; 171uy; 137uy; 176uy; 140uy; 165uy; 185uy; 179uy; 179uy; 181uy; 175uy; 159uy; 126uy; 87uy; 79uy; 84uy; 119uy; 205uy; 168uy; 179uy; 171uy; 171uy; 170uy; 114uy; 52uy; 77uy; 69uy; 91uy; 145uy; 45uy; 74uy; 135uy; 49uy; 54uy; 122uy; 87uy; 81uy; 167uy; 114uy; 99uy; 123uy; 58uy; 132uy; 82uy; 80uy; 104uy; 125uy; 142uy; 105uy;
//         232uy; 222uy; 146uy; 141uy; 168uy; 163uy; 144uy; 190uy; 179uy; 183uy; 184uy; 181uy; 175uy; 134uy; 72uy; 87uy; 83uy; 115uy; 203uy; 169uy; 181uy; 172uy; 174uy; 173uy; 119uy; 50uy; 78uy; 66uy; 82uy; 158uy; 121uy; 78uy; 156uy; 128uy; 125uy; 78uy; 63uy; 62uy; 176uy; 72uy; 78uy; 139uy; 44uy; 152uy; 38uy; 59uy; 54uy; 90uy; 145uy; 15uy;
//         233uy; 212uy; 137uy; 146uy; 152uy; 170uy; 155uy; 177uy; 191uy; 186uy; 187uy; 185uy; 180uy; 148uy; 71uy; 89uy; 85uy; 108uy; 195uy; 173uy; 185uy; 174uy; 177uy; 174uy; 128uy; 49uy; 77uy; 64uy; 77uy; 153uy; 132uy; 76uy; 154uy; 137uy; 141uy; 129uy; 89uy; 35uy; 168uy; 87uy; 22uy; 50uy; 28uy; 51uy; 38uy; 136uy; 102uy; 100uy; 153uy; 91uy;
//         233uy; 194uy; 140uy; 150uy; 150uy; 164uy; 179uy; 170uy; 198uy; 187uy; 189uy; 182uy; 182uy; 158uy; 79uy; 91uy; 91uy; 106uy; 185uy; 179uy; 179uy; 176uy; 179uy; 175uy; 128uy; 54uy; 75uy; 64uy; 79uy; 152uy; 125uy; 33uy; 61uy; 61uy; 64uy; 77uy; 107uy; 125uy; 142uy; 129uy; 68uy; 107uy; 75uy; 116uy; 59uy; 74uy; 68uy; 83uy; 157uy; 107uy;
//         229uy; 179uy; 150uy; 156uy; 158uy; 162uy; 181uy; 180uy; 193uy; 196uy; 192uy; 184uy; 184uy; 163uy; 82uy; 95uy; 95uy; 114uy; 191uy; 175uy; 178uy; 172uy; 178uy; 170uy; 121uy; 61uy; 77uy; 67uy; 79uy; 160uy; 116uy; 74uy; 92uy; 91uy; 92uy; 83uy; 72uy; 102uy; 150uy; 131uy; 111uy; 116uy; 90uy; 127uy; 66uy; 61uy; 56uy; 82uy; 136uy; 98uy;
//         209uy; 174uy; 169uy; 170uy; 174uy; 177uy; 185uy; 190uy; 196uy; 200uy; 197uy; 189uy; 184uy; 168uy; 84uy; 97uy; 98uy; 126uy; 202uy; 177uy; 185uy; 174uy; 173uy; 160uy; 109uy; 71uy; 75uy; 67uy; 84uy; 165uy; 137uy; 143uy; 139uy; 145uy; 144uy; 134uy; 135uy; 128uy; 132uy; 132uy; 121uy; 114uy; 105uy; 113uy; 104uy; 100uy; 98uy; 96uy; 96uy; 86uy;
//         192uy; 180uy; 180uy; 179uy; 179uy; 180uy; 181uy; 183uy; 189uy; 190uy; 194uy; 192uy; 192uy; 181uy; 90uy; 95uy; 101uy; 142uy; 209uy; 184uy; 188uy; 173uy; 171uy; 161uy; 106uy; 74uy; 77uy; 68uy; 85uy; 161uy; 138uy; 146uy; 144uy; 142uy; 147uy; 137uy; 140uy; 133uy; 127uy; 116uy; 104uy; 94uy; 88uy; 81uy; 78uy; 73uy; 72uy; 69uy; 69uy; 64uy;
//         193uy; 185uy; 185uy; 181uy; 180uy; 178uy; 177uy; 177uy; 182uy; 180uy; 182uy; 179uy; 179uy; 161uy; 89uy; 93uy; 103uy; 152uy; 211uy; 187uy; 195uy; 179uy; 170uy; 163uy; 103uy; 78uy; 79uy; 71uy; 85uy; 159uy; 145uy; 151uy; 144uy; 143uy; 147uy; 142uy; 138uy; 125uy; 112uy; 94uy; 81uy; 69uy; 63uy; 61uy; 61uy; 61uy; 62uy; 63uy; 63uy; 60uy;
//         194uy; 189uy; 189uy; 188uy; 186uy; 183uy; 179uy; 176uy; 180uy; 177uy; 174uy; 170uy; 162uy; 117uy; 76uy; 98uy; 100uy; 154uy; 210uy; 184uy; 195uy; 188uy; 176uy; 155uy; 102uy; 78uy; 80uy; 73uy; 90uy; 169uy; 155uy; 155uy; 139uy; 152uy; 152uy; 143uy; 137uy; 113uy; 92uy; 73uy; 63uy; 58uy; 59uy; 58uy; 60uy; 59uy; 63uy; 64uy; 64uy; 61uy;
//         194uy; 189uy; 192uy; 191uy; 188uy; 184uy; 182uy; 177uy; 175uy; 171uy; 163uy; 153uy; 134uy; 84uy; 82uy; 93uy; 96uy; 150uy; 209uy; 181uy; 194uy; 189uy; 182uy; 162uy; 97uy; 78uy; 79uy; 73uy; 96uy; 179uy; 151uy; 154uy; 138uy; 151uy; 152uy; 143uy; 125uy; 94uy; 78uy; 60uy; 57uy; 56uy; 57uy; 57uy; 60uy; 61uy; 60uy; 60uy; 65uy; 65uy;
//         192uy; 189uy; 192uy; 188uy; 186uy; 180uy; 176uy; 169uy; 164uy; 155uy; 148uy; 137uy; 112uy; 71uy; 86uy; 88uy; 94uy; 140uy; 206uy; 177uy; 191uy; 185uy; 182uy; 165uy; 95uy; 79uy; 79uy; 73uy; 98uy; 174uy; 145uy; 150uy; 137uy; 140uy; 133uy; 122uy; 94uy; 70uy; 62uy; 53uy; 55uy; 52uy; 53uy; 53uy; 55uy; 55uy; 55uy; 58uy; 61uy; 56uy;
//         202uy; 188uy; 184uy; 178uy; 173uy; 167uy; 166uy; 160uy; 159uy; 153uy; 148uy; 135uy; 98uy; 72uy; 85uy; 84uy; 87uy; 127uy; 204uy; 174uy; 188uy; 180uy; 179uy; 160uy; 87uy; 82uy; 77uy; 74uy; 81uy; 105uy; 95uy; 99uy; 94uy; 83uy; 61uy; 47uy; 38uy; 40uy; 44uy; 36uy; 37uy; 35uy; 37uy; 36uy; 37uy; 35uy; 36uy; 36uy; 38uy; 37uy;
//         213uy; 171uy; 175uy; 166uy; 166uy; 163uy; 164uy; 162uy; 161uy; 155uy; 151uy; 132uy; 88uy; 78uy; 83uy; 82uy; 86uy; 119uy; 201uy; 169uy; 182uy; 175uy; 175uy; 148uy; 80uy; 85uy; 76uy; 72uy; 60uy; 57uy; 57uy; 51uy; 49uy; 44uy; 38uy; 30uy; 31uy; 35uy; 37uy; 33uy; 32uy; 31uy; 32uy; 32uy; 32uy; 30uy; 32uy; 31uy; 32uy; 29uy;
// |]

let leftInput = [|
    127uy; 126uy; 123uy; 129uy;
    55uy; 49uy; 53uy; 58uy;
    48uy; 47uy; 50uy; 200uy;
|]

let rightInput = [|
    127uy; 127uy; 124uy; 130uy;
    53uy; 49uy; 54uy; 57uy;
    48uy; 201uy; 203uy; 199uy;
|]

let matchingParameters : Common.Parameters = {
        leftImage = leftInput
        rightImage = rightInput
        width = 4
        height = 3
        totalPixels = 4 * 3
        windowEdgeSize = 3
        maximumDisparity = 16
        zeroMean = true
}

let bpparameters : BeliefPropagation.BPParameters = {
    dataFunction = (Data.FHTruncatedLinear Smoothness.LAMBDA_FH Smoothness.TAU_FH)
    smoothnessFunction = (Smoothness.truncatedLinear Smoothness.D_FH)
    iterations = 20
}

let dataCosts = Data.computeDataCosts matchingParameters bpparameters.dataFunction
//printfn "%A" dataCosts
// Array.iteri (fun i d -> printf "(#%d = %d), " i (Array.length d)) dataCosts
// printfn " "
let smoothnessCosts = Smoothness.computeSmoothnessCosts matchingParameters bpparameters.smoothnessFunction
//printfn "%A" smoothnessCosts
//printfn "smooth dims are %d & %d" (Array2D.length1 smoothnessCosts) (Array2D.length2 smoothnessCosts)
let mutable messages1 = BeliefPropagation.initMessages matchingParameters // All messages will be 0 initially
//printfn "%A" messages1
let mutable messages2 = BeliefPropagation.initMessages matchingParameters
for i = 1 to bpparameters.iterations do
        BeliefPropagation.updateMessages matchingParameters.maximumDisparity dataCosts smoothnessCosts messages1 messages2
        let temp = messages1
        messages1 <- messages2
        messages2 <- temp
        printfn "\n_____________________ %d ___________________\n" i
        printfn "%A" messages1

// printfn "m1:\n%A\n" messages1
// printfn "m2:\n%A\n" messages2
// printfn "%A" (messages1 = messages2)

//let results = BeliefPropagation.computeFinalDisparities matchingParameters dataCosts messages1
//printfn "%A" results